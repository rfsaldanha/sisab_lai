suppressPackageStartupMessages({
  library(arrow)
  library(cli)
  library(DBI)
  library(dplyr)
  library(duckdb)
  library(fs)
  library(purrr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tibble)
})

# Input, output, and cache locations. The LAI CSV files are treated as static:
# once a file's path, size, and modification time match the cache, expensive
# header detection and row counting are skipped on later runs.
input_dir <- "data/lai"
output_dir <- "data/export"
data_output_dir <- path(output_dir, "data")
reports_output_dir <- path(output_dir, "reports")
cache_output_dir <- path(output_dir, "cache")
file_cache_path <- path(cache_output_dir, "sisab_lai_file_cache.rds")
data_file_prefix <- "sisab_saude_ciap_cid"

is_truthy <- function(x) {
  str_to_lower(x) %in% c("1", "true", "t", "yes", "y")
}

args <- commandArgs(trailingOnly = TRUE)
full_rebuild <- "--full" %in% args || is_truthy(Sys.getenv("SISAB_FULL_REBUILD", ""))

dir_create(c(data_output_dir, reports_output_dir, cache_output_dir))

csv_work_dir <- tempfile("sisab_lai_csv_")
dir_create(csv_work_dir)
on.exit(try(dir_delete(csv_work_dir), silent = TRUE), add = TRUE)

# Known source schemas observed in the LAI deliveries.
expected_old <- c(
  "NU_ANO_COMPETENCIA",
  "NU_COMPETENCIA",
  "CO_MUNICIPIO_IBGE",
  "TP_PCA",
  "PCA",
  "QT_ATENDIMENTOS"
)

expected_new <- c(
  "NU_ANO_COMPETENCIA",
  "NU_COMPETENCIA",
  "CO_MUNICIPIO_IBGE",
  "CID_CIAP",
  "QT_ATENDIMENTOS"
)

expected_excel_municipal <- c(
  "COMPETENCIA",
  "UF",
  "MUNICIPIO",
  "TP_CLASSIFICACAO",
  "CID_CIAP",
  "QTD_ATENDIMENTOS"
)

data_line_pattern <- "^\\s*\"?\\d{4}\"?\\s*,\\s*\"?\\d{6}\"?\\s*,"

# DuckDB COPY expects single-quoted paths. Escape any apostrophes defensively.
sql_quote <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

is_zip_path <- function(path) {
  str_ends(str_to_lower(as.character(path)), fixed(".zip"))
}

is_excel_path <- function(path) {
  lower_path <- str_to_lower(as.character(path))
  str_ends(lower_path, fixed(".xlsx")) | str_ends(lower_path, fixed(".xls"))
}

csv_name_from_path <- function(path) {
  str_remove(path_file(as.character(path)), fixed(".zip"))
}

source_file_from_path <- function(path) {
  if_else(
    is_zip_path(path),
    csv_name_from_path(path),
    path_file(as.character(path))
  )
}

# Source CSVs may be stored as .csv.zip to save disk space. When a script step
# needs the CSV bytes, extract just that member to a temporary working folder.
materialize_csv <- function(path) {
  path <- as.character(path)
  if (!is_zip_path(path)) {
    return(path)
  }

  expected_name <- csv_name_from_path(path)
  safe_prefix <- str_replace_all(path, "[^A-Za-z0-9._-]+", "_")
  target <- fs::path(csv_work_dir, paste0(safe_prefix, "__", expected_name))
  if (file_exists(target)) {
    return(target)
  }

  members <- tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(e) stop("Could not list ZIP archive: ", path, call. = FALSE)
  )
  csv_members <- members$Name[str_detect(members$Name, "[.]csv$")]
  member <- csv_members[path_file(csv_members) == expected_name][1]
  if (is.na(member)) {
    member <- csv_members[1]
  }
  if (is.na(member)) {
    stop("No CSV member found inside ZIP archive: ", path, call. = FALSE)
  }

  extracted <- utils::unzip(path, files = member, exdir = csv_work_dir, junkpaths = TRUE, overwrite = TRUE)
  extracted <- extracted[1]
  if (!identical(path_norm(extracted), path_norm(target))) {
    if (file_exists(target)) {
      file_delete(target)
    }
    file_move(extracted, target)
  }
  target
}

zip_csv_file <- function(csv_path) {
  csv_path <- as.character(csv_path)
  zip_path <- paste0(csv_path, ".zip")
  if (file_exists(zip_path)) {
    file_delete(zip_path)
  }

  status <- system2("zip", c("-9", "-j", zip_path, csv_path), stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L)) {
    stop("Could not ZIP CSV file: ", csv_path, call. = FALSE)
  }
  file_delete(csv_path)
  zip_path
}

# SISAB data files are preceded by SQL*Plus command output. This finds the real
# CSV header and identifies which LAI schema the file uses.
detect_header <- function(path) {
  if (is_excel_path(path)) {
    excel_header <- tryCatch(
      names(read_excel(path, n_max = 0, .name_repair = "minimal")),
      error = function(e) character()
    )

    if (length(excel_header) == 0) {
      return(list(
        header_line = NA_integer_,
        schema = NA_character_,
        reason = "empty_or_unreadable"
      ))
    }

    if (!all(expected_excel_municipal %in% excel_header)) {
      return(list(
        header_line = 1L,
        schema = NA_character_,
        reason = "unsupported_schema"
      ))
    }

    return(list(
      header_line = 1L,
      schema = "excel_municipal_cid_ciap",
      reason = NA_character_
    ))
  }

  csv_path <- materialize_csv(path)
  preview <- tryCatch(
    readLines(csv_path, n = 100, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character()
  )

  if (length(preview) == 0) {
    return(list(
      header_line = NA_integer_,
      schema = NA_character_,
      reason = "empty_or_unreadable"
    ))
  }

  header_line <- which(str_detect(preview, "NU_ANO_COMPETENCIA") &
    str_detect(preview, "NU_COMPETENCIA"))[1]

  if (is.na(header_line)) {
    return(list(
      header_line = NA_integer_,
      schema = NA_character_,
      reason = "recognized_header_not_found"
    ))
  }

  header <- preview[[header_line]]

  if (str_detect(header, "TP_PCA") && str_detect(header, "\"PCA\"|,PCA,|,PCA$")) {
    schema <- "explicit_tp_pca"
  } else if (str_detect(header, "CID_CIAP")) {
    schema <- "combined_cid_ciap"
  } else {
    schema <- NA_character_
  }

  if (is.na(schema)) {
    return(list(
      header_line = header_line,
      schema = NA_character_,
      reason = "unsupported_schema"
    ))
  }

  list(header_line = header_line, schema = schema, reason = NA_character_)
}

# Fast path: count all lines and subtract the SQL*Plus preamble/header/footer.
# Fallback: scan data-looking rows if fast line counting fails for any file.
count_data_rows <- function(path, header_line) {
  if (is_excel_path(path)) {
    return(tryCatch(
      as.integer(nrow(read_excel(path, col_types = "text"))),
      error = function(e) NA_integer_
    ))
  }

  csv_path <- materialize_csv(path)
  total_lines <- tryCatch(
    R.utils::countLines(csv_path),
    error = function(e) NA_integer_
  )

  if (!is.na(total_lines)) {
    return(as.integer(max(total_lines - header_line - 1L, 0L)))
  }

  total <- 0L
  callback <- SideEffectChunkCallback$new(function(x, pos) {
    if (pos == 1 && length(x) >= header_line) {
      x <- x[-seq_len(header_line)]
    }
    if (length(x) > 0) {
      total <<- total + sum(str_detect(x, data_line_pattern), na.rm = TRUE)
    }
  })

  tryCatch(
    {
      read_lines_chunked(
        file = csv_path,
        callback = callback,
        chunk_size = 100000,
        progress = FALSE
      )
      total
    },
    error = function(e) NA_integer_
  )
}

# Read one selected monthly file and normalize both known LAI schemas to the
# final tidy export shape. Provenance stays in the data, but source_schema is
# only kept in the reports/selection metadata.
read_sisab_file <- function(path, header_line, schema, source_request, source_file) {
  if (schema == "excel_municipal_cid_ciap") {
    raw <- read_excel(
      path = path,
      col_types = "text",
      .name_repair = "minimal"
    )
  } else {
    csv_path <- materialize_csv(path)
    raw <- suppressWarnings(read_csv(
      file = csv_path,
      skip = header_line - 1L,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "minimal"
    ))
  }

  if (schema == "explicit_tp_pca") {
    missing_cols <- setdiff(expected_old, names(raw))
    if (length(missing_cols) > 0) {
      stop("missing expected columns: ", paste(missing_cols, collapse = ", "))
    }

    raw <- raw |>
      filter(
        str_detect(NU_ANO_COMPETENCIA, "^\\d{4}$"),
        str_detect(NU_COMPETENCIA, "^\\d{6}$")
      )

    # Older files store the code type and code in separate columns.
    out <- raw |>
      transmute(
        ano_competencia = parse_integer(NU_ANO_COMPETENCIA),
        competencia = str_pad(NU_COMPETENCIA, width = 6, side = "left", pad = "0"),
        co_municipio_ibge = CO_MUNICIPIO_IBGE,
        uf = NA_character_,
        municipio = NA_character_,
        tp_codigo = TP_PCA,
        codigo = PCA,
        qt_atendimentos = parse_integer(QT_ATENDIMENTOS),
        source_schema = schema
      )
  } else if (schema == "combined_cid_ciap") {
    missing_cols <- setdiff(expected_new, names(raw))
    if (length(missing_cols) > 0) {
      stop("missing expected columns: ", paste(missing_cols, collapse = ", "))
    }

    raw <- raw |>
      filter(
        str_detect(NU_ANO_COMPETENCIA, "^\\d{4}$"),
        str_detect(NU_COMPETENCIA, "^\\d{6}$")
      )

    # Newer files combine code and type as CID_CIAP, where suffix 1 means CID
    # and suffix 4 means CIAP.
    out <- raw |>
      mutate(
        cid_ciap_suffix = str_match(CID_CIAP, "^(.*)-([^-]+)$")[, 3],
        cid_ciap_value = str_match(CID_CIAP, "^(.*)-([^-]+)$")[, 2]
      ) |>
      transmute(
        ano_competencia = parse_integer(NU_ANO_COMPETENCIA),
        competencia = str_pad(NU_COMPETENCIA, width = 6, side = "left", pad = "0"),
        co_municipio_ibge = CO_MUNICIPIO_IBGE,
        uf = NA_character_,
        municipio = NA_character_,
        tp_codigo = case_when(
          cid_ciap_suffix == "1" ~ "CID",
          cid_ciap_suffix == "4" ~ "CIAP",
          TRUE ~ NA_character_
        ),
        codigo = cid_ciap_value,
        qt_atendimentos = parse_integer(QT_ATENDIMENTOS),
        source_schema = schema
      )
  } else if (schema == "excel_municipal_cid_ciap") {
    missing_cols <- setdiff(expected_excel_municipal, names(raw))
    if (length(missing_cols) > 0) {
      stop("missing expected columns: ", paste(missing_cols, collapse = ", "))
    }

    raw <- raw |>
      filter(str_detect(COMPETENCIA, "^\\d{6}"))

    out <- raw |>
      mutate(
        cid_ciap_suffix = str_match(CID_CIAP, "^(.*)-([^-]+)")[, 3],
        cid_ciap_value = str_match(CID_CIAP, "^(.*)-([^-]+)")[, 2]
      ) |>
      transmute(
        ano_competencia = parse_integer(substr(COMPETENCIA, 1, 4)),
        competencia = str_pad(COMPETENCIA, width = 6, side = "left", pad = "0"),
        co_municipio_ibge = NA_character_,
        uf = UF,
        municipio = MUNICIPIO,
        tp_codigo = case_when(
          TP_CLASSIFICACAO %in% c("CID", "CIAP") ~ TP_CLASSIFICACAO,
          cid_ciap_suffix == "1" ~ "CID",
          cid_ciap_suffix == "4" ~ "CIAP",
          TRUE ~ NA_character_
        ),
        codigo = coalesce(cid_ciap_value, CID_CIAP),
        qt_atendimentos = parse_integer(QTD_ATENDIMENTOS),
        source_schema = schema
      )
  } else {
    stop("unsupported schema: ", schema)
  }

  out |>
    filter(str_detect(competencia, "^\\d{6}$")) |>
    mutate(
      competencia_date = as.Date(paste0(substr(competencia, 1, 4), "-", substr(competencia, 5, 6), "-01")),
      source_request = source_request,
      source_file = source_file,
      .after = qt_atendimentos
    ) |>
    select(
      ano_competencia,
      competencia,
      competencia_date,
      co_municipio_ibge,
      uf,
      municipio,
      tp_codigo,
      codigo,
      qt_atendimentos,
      source_request,
      source_file
    )
}

# Discover the current LAI file universe. New monthly requests can be added as
# new data/lai/pedido_*/csv or data/lai/pedido_*/excel folders without
# changing the script. CSVs can be stored either plain, as .csv.zip archives, or
# as generic .zip archives with a CSV member. Excel files can be .xlsx or .xls.
source_files <- dir_ls(
  path = input_dir,
  recurse = TRUE,
  type = "file",
  regexp = "/(csv|excel)/.*([.]csv([.]zip)?|[.]zip|[.]xlsx?|[.]xls)"
)

if (length(source_files) == 0) {
  stop("No CSV or Excel files found under ", input_dir, call. = FALSE)
}

cli_h1("SISAB LAI import")
cli_alert_info("Input folder: {.path {input_dir}}")
cli_alert_info("Data output folder: {.path {data_output_dir}}")
cli_alert_info("Reports output folder: {.path {reports_output_dir}}")
cli_alert_info("Found {.strong {length(source_files)}} source files.")

# Build the file-level inventory used for validation, overlap resolution, and
# cache matching. The cache key changes if the file is replaced or edited.
inventory <- tibble(path = as.character(source_files)) |>
  mutate(
    source_request = str_extract(path, "pedido_[^/]+"),
    source_file = source_file_from_path(path),
    source_format = if_else(is_excel_path(path), "excel", "csv"),
    competencia = str_extract(source_file, "\\d{6}"),
    ano_competencia = parse_integer(substr(competencia, 1, 4)),
    mes_competencia = parse_integer(substr(competencia, 5, 6)),
    file_size = as.numeric(file_size(path)),
    file_mtime = file_info(path)$modification_time,
    cache_key = paste(path, file_size, file_mtime, sep = "|")
  ) |>
  mutate(
    metadata_valid = !is.na(source_request) &
      !is.na(competencia) &
      !is.na(ano_competencia) &
      !is.na(mes_competencia) &
      between(mes_competencia, 1L, 12L)
  )

cli_h2("Inspecting files")
# Load previous inspection results. Cached rows avoid rereading unchanged LAI
# files just to rediscover headers and row counts.
file_cache <- if (file_exists(file_cache_path)) {
  readRDS(file_cache_path)
} else {
  tibble(
    path = character(),
    cache_key = character(),
    source_request = character(),
    source_file = character(),
    source_format = character(),
    competencia = character(),
    ano_competencia = integer(),
    mes_competencia = integer(),
    file_size = double(),
    file_mtime = as.POSIXct(character()),
    metadata_valid = logical(),
    header_line = integer(),
    source_schema = character(),
    header_reason = character(),
    data_rows = integer(),
    valid = logical(),
    invalid_reason = character(),
    cache_updated_at = as.POSIXct(character())
  )
}

# Reattach cached validation details for unchanged files.
cached_inspection <- file_cache |>
  filter(cache_key %in% inventory$cache_key) |>
  select(
    cache_key,
    header_line,
    source_schema,
    header_reason,
    data_rows,
    valid,
    invalid_reason
  ) |>
  distinct(cache_key, .keep_all = TRUE) |>
  mutate(cache_hit = TRUE)

inventory <- inventory |>
  left_join(cached_inspection, by = "cache_key")

cache_misses <- which(is.na(inventory$cache_hit))
cli_alert_info("Reusing cached inspection for {.strong {sum(!is.na(inventory$cache_hit))}} unchanged files.")
cli_alert_info("Inspecting {.strong {length(cache_misses)}} new or changed files.")

# Only cache misses need file inspection. This is the slow part on first run and
# should be small on later monthly updates.
if (length(cache_misses) > 0) {
  cli_alert_info("Detecting source headers.")
}
header_info <- vector("list", length(cache_misses))
for (j in cli_progress_along(seq_along(cache_misses), "Detecting headers")) {
  i <- cache_misses[[j]]
  header_info[[j]] <- detect_header(inventory$path[[i]])
}
cli_progress_done()

if (length(cache_misses) > 0) {
  inventory$header_line[cache_misses] <- map_int(header_info, "header_line", .default = NA_integer_)
  inventory$source_schema[cache_misses] <- map_chr(header_info, "schema", .default = NA_character_)
  inventory$header_reason[cache_misses] <- map_chr(header_info, "reason", .default = NA_character_)
}

countable <- cache_misses[!is.na(inventory$header_line[cache_misses])]
if (length(countable) > 0) {
  cli_alert_info("Counting data rows in files with recognized headers.")
}
for (j in cli_progress_along(seq_along(countable), "Counting rows")) {
  i <- countable[[j]]
  inventory$data_rows[[i]] <- count_data_rows(inventory$path[[i]], inventory$header_line[[i]])
}
cli_progress_done()

inventory <- inventory |>
  mutate(
    valid = metadata_valid &
      is.na(header_reason) &
      !is.na(data_rows) &
      data_rows > 0L,
    invalid_reason = case_when(
      !metadata_valid ~ "invalid_path_or_competencia_metadata",
      !is.na(header_reason) ~ header_reason,
      is.na(data_rows) ~ "could_not_count_data_rows",
      data_rows == 0L ~ "zero_data_rows",
      TRUE ~ NA_character_
    )
  )

# Persist inspection metadata for future runs. This cache is intentionally a
# report-side optimization, not an input source of truth.
cache_columns <- c(
  "path",
  "cache_key",
  "source_request",
  "source_file",
  "source_format",
  "competencia",
  "ano_competencia",
  "mes_competencia",
  "file_size",
  "file_mtime",
  "metadata_valid",
  "header_line",
  "source_schema",
  "header_reason",
  "data_rows",
  "valid",
  "invalid_reason"
)

updated_cache <- bind_rows(
  file_cache |> filter(!cache_key %in% inventory$cache_key),
  inventory |> select(all_of(cache_columns))
) |>
  mutate(cache_updated_at = Sys.time())

saveRDS(updated_cache, file_cache_path)
cli_alert_success("Updated file inspection cache: {.path {file_cache_path}}")

cli_alert_success("File inspection complete.")
cli_ul(c(
  "Valid candidates: {sum(inventory$valid)}",
  "Invalid files: {sum(!inventory$valid)}",
  "Competence range observed: {min(inventory$competencia, na.rm = TRUE)} to {max(inventory$competencia, na.rm = TRUE)}"
))

cli_h2("Resolving overlaps")
# Multiple LAI requests can cover the same competencia. Among valid candidates,
# keep the most complete file by row count, then file size, then request id.
valid_inventory <- inventory |>
  filter(valid) |>
  arrange(competencia, desc(data_rows), desc(file_size), desc(source_request))

selected_files <- valid_inventory |>
  group_by(competencia) |>
  mutate(
    selected = row_number() == 1L,
    selection_status = if_else(selected, "selected", "superseded")
  ) |>
  ungroup()

selected_paths <- selected_files |>
  filter(selected) |>
  select(
    competencia,
    ano_competencia,
    path,
    source_request,
    source_file,
    source_format,
    source_schema,
    header_line,
    data_rows,
    file_size,
    file_mtime,
    cache_key,
    selection_status
  )

cli_alert_success("Selected {.strong {nrow(selected_paths)}} monthly files after overlap resolution.")
cli_alert_info("Superseded valid overlaps: {.strong {sum(selected_files$selection_status == 'superseded')}}")

# Missing months are computed over the observed range, so future monthly gaps
# appear automatically in the diagnostics.
all_months <- if (any(!is.na(inventory$competencia))) {
  observed_dates <- as.Date(paste0(
    substr(inventory$competencia, 1, 4),
    "-",
    substr(inventory$competencia, 5, 6),
    "-01"
  ))
  observed_dates <- observed_dates[!is.na(observed_dates)]
  seq.Date(min(observed_dates), max(observed_dates), by = "month")
} else {
  as.Date(character())
}

missing_months <- tibble(
  competencia = format(all_months, "%Y%m"),
  ano_competencia = parse_integer(format(all_months, "%Y")),
  competencia_date = all_months
) |>
  anti_join(selected_paths |> select(competencia), by = "competencia")

cli_alert_info("Missing months in observed range: {.strong {nrow(missing_months)}}")

inventory_out <- inventory |>
  left_join(
    selected_files |> select(path, selected, selection_status),
    by = "path"
  ) |>
  mutate(
    selected = coalesce(selected, FALSE),
    selection_status = case_when(
      selected ~ "selected",
      valid ~ selection_status,
      TRUE ~ "invalid"
    )
  )

# Reports preserve source_schema and selection metadata even though the final
# yearly data exports omit source_schema. The previous selected-files report is
# also the baseline for deciding which annual exports need rebuilding.
inventory_report_path <- path(reports_output_dir, "sisab_lai_file_inventory.csv")
selected_report_path <- path(reports_output_dir, "sisab_lai_selected_files.csv")
invalid_report_path <- path(reports_output_dir, "sisab_lai_invalid_files.csv")
missing_report_path <- path(reports_output_dir, "sisab_lai_missing_months.csv")

previous_selected_paths <- if (file_exists(paste0(selected_report_path, ".zip"))) {
  suppressMessages(read_csv(paste0(selected_report_path, ".zip"), show_col_types = FALSE))
} else {
  tibble()
}

current_years <- sort(unique(selected_paths$ano_competencia))
previous_years <- if ("ano_competencia" %in% names(previous_selected_paths)) {
  sort(unique(previous_selected_paths$ano_competencia))
} else {
  integer()
}

selected_compare_columns <- c(
  "competencia",
  "ano_competencia",
  "path",
  "source_schema",
  "header_line",
  "data_rows",
  "file_size"
)
selected_compare_columns <- intersect(
  selected_compare_columns,
  intersect(names(selected_paths), names(previous_selected_paths))
)

if (nrow(previous_selected_paths) == 0 || length(selected_compare_columns) == 0) {
  changed_current_selection <- selected_paths
  changed_previous_selection <- previous_selected_paths
} else {
  current_selected_compare <- selected_paths |>
    mutate(across(all_of(selected_compare_columns), as.character))
  previous_selected_compare <- previous_selected_paths |>
    mutate(across(all_of(selected_compare_columns), as.character))

  changed_current_selection <- anti_join(
    current_selected_compare,
    previous_selected_compare,
    by = selected_compare_columns
  )
  changed_previous_selection <- anti_join(
    previous_selected_compare,
    current_selected_compare,
    by = selected_compare_columns
  )
}

changed_years <- sort(unique(as.integer(c(
  changed_current_selection$ano_competencia,
  changed_previous_selection$ano_competencia
))))
removed_years <- setdiff(previous_years, current_years)
missing_output_years <- current_years[
  !file_exists(path(data_output_dir, paste0(data_file_prefix, "_", current_years, ".csv.zip"))) |
    !file_exists(path(data_output_dir, paste0(data_file_prefix, "_", current_years, ".parquet")))
]

years_to_rebuild <- if (full_rebuild) {
  current_years
} else {
  sort(unique(c(changed_years, missing_output_years)))
}
years_to_delete <- sort(unique(c(years_to_rebuild, removed_years)))

cli_h2("Planning exports")
if (full_rebuild) {
  cli_alert_info("Full rebuild requested with --full or SISAB_FULL_REBUILD.")
}
cli_alert_info("Years with changed selected inputs: {.strong {length(changed_years)}}")
cli_alert_info("Years with missing output files: {.strong {length(missing_output_years)}}")
cli_alert_info("Years queued for rebuild: {.strong {length(years_to_rebuild)}}")

cli_h2("Writing diagnostics")
write_csv(inventory_out, inventory_report_path)
inventory_report_zip <- zip_csv_file(inventory_report_path)
cli_alert_success("Wrote {.path {inventory_report_zip}}")
write_csv(selected_paths, selected_report_path)
selected_report_zip <- zip_csv_file(selected_report_path)
cli_alert_success("Wrote {.path {selected_report_zip}}")
write_csv(inventory |> filter(!valid), invalid_report_path)
invalid_report_zip <- zip_csv_file(invalid_report_path)
cli_alert_success("Wrote {.path {invalid_report_zip}}")
write_csv(missing_months, missing_report_path)
missing_report_zip <- zip_csv_file(missing_report_path)
cli_alert_success("Wrote {.path {missing_report_zip}}")

legacy_root_files <- dir_ls(
  output_dir,
  type = "file",
  regexp = "sisab_lai_.*[.](csv([.]zip)?|parquet)$",
  fail = FALSE
)
if (length(legacy_root_files) > 0) {
  cli_alert_info("Removing {length(legacy_root_files)} legacy root-level export/report files from {.path {output_dir}}.")
  file_delete(legacy_root_files)
}

if (length(years_to_delete) > 0) {
  year_pattern <- paste(years_to_delete, collapse = "|")
  yearly_outputs_to_delete <- dir_ls(
    data_output_dir,
    regexp = paste0(
      "(sisab_lai|",
      data_file_prefix,
      ")_(",
      year_pattern,
      ")[.](csv([.]zip)?|parquet)$"
    ),
    fail = FALSE
  )
  if (length(yearly_outputs_to_delete) > 0) {
    cli_alert_info("Removing {length(yearly_outputs_to_delete)} stale yearly export files.")
    file_delete(yearly_outputs_to_delete)
  }
}

cli_h2("Exporting yearly files")
if (length(years_to_rebuild) == 0) {
  cli_alert_success("No yearly exports need rebuilding.")
} else {
  # DuckDB is used as a temporary writer so each yearly CSV/Parquet pair is
  # produced consistently without keeping all years in memory at once.
  duckdb_path <- tempfile("sisab_lai_build_", fileext = ".duckdb")

  con <- dbConnect(duckdb(), dbdir = duckdb_path)
  on.exit({
    try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)
    try(file_delete(duckdb_path), silent = TRUE)
  }, add = TRUE)

  for (year in years_to_rebuild) {
    dbExecute(con, "DROP TABLE IF EXISTS sisab_year")

    year_files <- selected_paths |>
      filter(ano_competencia == year) |>
      arrange(competencia)

    cli_alert_info("Year {.strong {year}}: importing {nrow(year_files)} selected monthly files.")
    wrote_any <- FALSE
    for (i in cli_progress_along(seq_len(nrow(year_files)), paste("Importing", year))) {
      cli_progress_message("Reading {year_files$competencia[[i]]}: {year_files$source_file[[i]]}")
      month_data <- read_sisab_file(
        path = year_files$path[[i]],
        header_line = year_files$header_line[[i]],
        schema = year_files$source_schema[[i]],
        source_request = year_files$source_request[[i]],
        source_file = year_files$source_file[[i]]
      ) |>
        filter(competencia == year_files$competencia[[i]])

      if (nrow(month_data) == 0) {
        warning("Selected file produced zero rows after normalization: ", year_files$path[[i]], call. = FALSE)
        next
      }

      dbWriteTable(
        conn = con,
        name = "sisab_year",
        value = month_data,
        append = wrote_any
      )
      wrote_any <- TRUE
    }
    cli_progress_done()

    if (!dbExistsTable(con, "sisab_year")) {
      warning("No rows exported for year ", year, call. = FALSE)
      next
    }

    csv_path <- path(data_output_dir, paste0(data_file_prefix, "_", year, ".csv"))
    parquet_path <- path(data_output_dir, paste0(data_file_prefix, "_", year, ".parquet"))

    cli_alert_info("Year {.strong {year}}: writing CSV export.")
    dbExecute(
      con,
      paste0(
        "COPY (SELECT * FROM sisab_year ORDER BY competencia, co_municipio_ibge, tp_codigo, codigo) TO ",
        sql_quote(csv_path),
        " (FORMAT CSV, HEADER TRUE)"
      )
    )
    csv_zip_path <- zip_csv_file(csv_path)
    cli_alert_info("Year {.strong {year}}: writing Parquet export.")
    dbExecute(
      con,
      paste0(
        "COPY (SELECT * FROM sisab_year ORDER BY competencia, co_municipio_ibge, tp_codigo, codigo) TO ",
        sql_quote(parquet_path),
        " (FORMAT PARQUET)"
      )
    )
    cli_alert_success("Year {.strong {year}} complete: {.path {csv_zip_path}} and {.path {parquet_path}}")
  }
}

cli_h2("Done")
cli_alert_success("Data files written to {.path {data_output_dir}}.")
cli_alert_success("Report files written to {.path {reports_output_dir}}.")
