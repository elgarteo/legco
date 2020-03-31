# Formulate legco_section_type
legco_section_type <- legco::legco_api("hansard", "Sections")

legco_section_type <- legco_section_type[c("SectionCode", "NameEng", "NameChi")]

for (i in 1:nrow(legco_section_type)) {
  if (!grepl(",", legco_section_type$SectionCode[i])) next
  section_codes <- strsplit(legco_section_type$SectionCode[i], ",")[[1]]
  legco_section_type$SectionCode[i] <- section_codes[1]
  section_codes <- section_codes[-1]
  tmp <- data.frame(SectionCode = section_codes,
                    NameEng = rep(legco_section_type$NameEng[i], length(section_codes)),
                    NameChi = rep(legco_section_type$NameChi[i], length(section_codes)),
                    stringsAsFactors = FALSE)
  legco_section_type <- rbind(legco_section_type, tmp)
}

legco_section_type <- legco_section_type[order(legco_section_type$NameChi), ]
rownames(legco_section_type) <- 1:nrow(legco_section_type)

usethis::use_data(legco_section_type, overwrite = TRUE)
