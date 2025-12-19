library(pdftools)
library(tesseract)
library(magick)

scans_folder <- "C:/Users/Miss Victoria/Documents/scans"
output_folder <- "ocr_results_txt"
dir.create(output_folder, showWarnings = FALSE)

files <- list.files(scans_folder, pattern="\\.pdf$", 
                    full.names=TRUE, ignore.case=TRUE)
cat("Найдено PDF-файлов:", length(files), "\n")

rus_engine <- tesseract("rus")
all_texts <- character(length(files))

for (i in seq_along(files)) {
  current_file <- files[i]
  cat("[", i, "/", length(files), "] ", basename(current_file), "\n", sep="")
  
  images <- pdftools::pdf_convert(current_file, format = "png", dpi = 200)
  
  text_pages <- sapply(images, function(img) {
    ocr(image_read(img), engine = rus_engine)
  })
  
  unlink(images)
  
  full_text <- paste(text_pages, collapse = "\n")
  all_texts[i] <- full_text
  
  txt_filename <- gsub("\\.pdf$", ".txt", basename(current_file), ignore.case = TRUE)
  txt_path <- file.path(output_folder, txt_filename)
  writeLines(full_text, txt_path, useBytes = TRUE)
  
  cat("   → ", nchar(full_text), " символов → ", txt_filename, "\n", sep="")
}

corpus_df <- data.frame(
  doc_id = seq_along(files),
  file_name = basename(files),
  text = all_texts,
  stringsAsFactors = FALSE
)

write.csv(corpus_df, "corpus_ocr_results.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

cat("Обработано файлов:", length(files), "\n")
cat("Сохранено TXT-файлов в папке '", output_folder, "'\n", sep="")
cat("Сводная таблица: 'corpus_ocr_results.csv'\n")
cat("Всего символов распознано:", sum(nchar(all_texts)), "\n")

library(stringr)

clean_folder <- "ocr_clean"
dir.create(clean_folder, showWarnings = FALSE)

clean_text_smart <- function(text) {
  original_length <- nchar(text)
  english_words <- str_extract_all(text, "\\b[a-zA-Z]+\\b")[[1]]
  
  if (length(english_words) > 0) {
    for (i in seq_along(english_words)) {
      placeholder <- paste0("@@ENGLISH", i, "@@")
      text <- str_replace(text, english_words[i], placeholder)
    }
  }
  
  text <- str_replace_all(text, "r", "г")
  text <- str_replace_all(text, "y", "у")
  text <- str_replace_all(text, "c", "с")
  text <- str_replace_all(text, "a", "а")
  text <- str_replace_all(text, "e", "е")
  text <- str_replace_all(text, "o", "о")
  
  if (length(english_words) > 0) {
    for (i in seq_along(english_words)) {
      placeholder <- paste0("@@ENGLISH", i, "@@")
      text <- str_replace(text, placeholder, english_words[i])
    }
  }
  
  text <- str_remove_all(text, "[0-9]+")
  text <- str_remove_all(text, "\\*+")
  text <- str_remove_all(text, "\\-{3,}")
  text <- str_replace_all(text, " +", " ")
  text <- str_trim(text)
  
  return(list(
    text = text,
    removed = original_length - nchar(text)
  ))
}

results <- lapply(corpus_df$text, clean_text_smart)
corpus_df$clean_text <- sapply(results, function(x) x$text)
removed_counts <- sapply(results, function(x) x$removed)

for (i in 1:nrow(corpus_df)) {
  clean_filename <- gsub("\\.pdf$", "_clean.txt", corpus_df$file_name[i], ignore.case = TRUE)
  clean_path <- file.path(clean_folder, clean_filename)
  writeLines(corpus_df$clean_text[i], clean_path, useBytes = TRUE)
}

cat("Всего документов:", nrow(corpus_df), "\n")
cat("Символов ДО очистки:", sum(nchar(corpus_df$text)), "\n")
cat("Символов ПОСЛЕ очистки:", sum(nchar(corpus_df$clean_text)), "\n")
cat("Всего удалено символов:", sum(removed_counts), "\n")
cat("Процент удалённого:", round(sum(removed_counts)/sum(nchar(corpus_df$text))*100, 1), "%\n")
cat("Очищенные тексты сохранены в папку:", normalizePath(clean_folder), "\n")

library(udpipe)
library(dplyr)

model <- udpipe_load_model("russian-syntagrus-ud-2.5-191206.udpipe")
full_text <- paste(corpus_df$clean_text, collapse = " ")
annotated <- udpipe_annotate(model, x = full_text)
annotated_df <- as.data.frame(annotated)

stopwords_ru <- c("и", "в", "не", "на", "я", "с", "что", "а", "по", "как", "это", "все", "она", "так", "его", "но", "да", "вы", "уж", "за", "бы", "же", "для", "из", "ли", "же", "вот", "о", "мне", "ни", "нет", "если", "у", "ты", "к", "до", "был", "он", "от", "или", "мы", "их", "чем", "была", "со", "во", "без", "будь", "то", "себя", "под", "год", "раз", "тоже", "очень", "может", "вот", "там", "ещё", "уже", "ну", "даже")

freq_dict <- annotated_df %>%
  filter(
    upos %in% c("NOUN", "ADJ", "VERB", "ADV"),
    !lemma %in% stopwords_ru,
    nchar(lemma) > 2,
    !grepl("[0-9a-zA-Z]", lemma)
  ) %>%
  count(lemma, sort = TRUE) %>%
  rename(word = lemma, frequency = n)

cat("Топ-20 самых частых слов:\n")
print(head(freq_dict, 20))
cat("\nВсего уникальных лемм:", nrow(freq_dict), "\n")
cat("Всего слов в корпусе:", nrow(annotated_df), "\n")

write.csv(freq_dict, "frequency_dictionary.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Частотный словарь сохранён в 'frequency_dictionary.csv'\n")
