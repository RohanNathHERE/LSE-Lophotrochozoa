# Load necessary libraries
library(ggplot2)
library(ggnewscale)  # Allows using multiple fill scales in a single ggplot
library(reshape2)    # For data reshaping

# Define the color palette for the heatmap
breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
colors <- c("#f7f9cb", "#c7edc0", "#92e0c3", "#56d0d0", "#00bdde", "#00afef", 
            "#539df4", "#9085e8", "#cd6ace", "#f54a9d", "#ff3d5e", "#f0550d", "#fc8d59")

# Target species (for figure 1)
target_words <- c(
  "Epimenia_babai", "Acanthochitona_crinita", "Acanthopleura_granulata", "Mopalia_swanii", 
  "Nautilus_pompilius", "Argonauta_argo", "Octopus_vulgaris", "Japetella_diaphana", 
  "Sepia_pharaonis", "Euprymna_scolopes", "Watasenia_scintillans", "Architeuthis_dux", 
  "Laevipilina_antarctica", "Gadila_tolmiei", "Alviniconcha_marisindica", "Batillaria_attramentaria", 
  "Melanoides_tuberculata", "Babylonia_areolata", "Conus_consors", "Rapana_venosa", 
  "Pomacea_canaliculata", "Chromodoris_orientalis", "Tritonia_tetraquetra", "Dirona_pellucida", 
  "Aplysia_californica", "Arion_vulgaris", "Oreohelix_idahoensis", "Biomphalaria_glabrata", 
  "Anisus_vortex", "Lymnaea_stagnalis", "Physella_acuta", "Elysia_marginata", "Plakobranchus_ocellatus", 
  "Gibbula_magus", "Haliotis_rufescens", "Dracogyra_subfuscus", "Gigantopelta_aegis", 
  "Patella_depressa", "Lottia_gigantea", "Potamilus_streckersoni", "Margaritifera_margaritifera", 
  "Panopea_generosa", "Mya_arenaria", "Mercenaria_mercenaria", "Archivesica_marissinica", 
  "Fragum_fragum", "Conchocele_bisecta", "Pecten_maximus", "Crassostrea_gigas", "Pinctada_fucata", 
  "Scapharca_broughtonii", "Bathymodiolus_platifrons", "Mytilus_edulis", "Laternula_elliptica", 
  "Pedicellina_cernua", "Bugula_neritina", "Cryptosula_pallasiana", "Phoronis_australis", 
  "Lingula_anatina", "Lineus_longissimus", "Capitella_teleta", "Sthenelais_limicola", "Alitta_virens", 
  "Acholoe_squamosa", "Amphiduros_pacificus", "Dimorphilus_gyrociliatus", "Riftia_pachyptila", 
  "Owenia_fusiformis", "Hydroides_elegans", "Paralvinella_palmiformis", "Metaphire_vulgaris", 
  "Aporrectodea_caliginosa", "Hirudo_medicinalis", "Helobdella_robusta", "Sipunculus_nudus", 
  "Girardia_tigrina", "Macrostomum_lignano", "Prostheceraeus_crozeri", "Protopolystoma_xenopodis", 
  "Gyrodactylus_bullatarudis", "Fasciola_gigantica", "Clonorchis_sinensis", "Schistosoma_haematobium", 
  "Cardicola_forsteri", "Dibothriocephalus_latus", "Echinococcus_granulosus", "Moniezia_expansa", 
  "Hymenolepis_diminuta", "Proteocephalus_fallax"
)

# Below are species lists categorized by taxonomic groups

# Gastropoda species
target_words <- c(
  "Alviniconcha_marisindica", "Batillaria_attramentaria", "Melanoides_tuberculata", 
  "Babylonia_areolata", "Conus_betulinus", "Conus_consors", "Conus_tribblei", 
  "Rapana_venosa", "Pomacea_canaliculata", "Chromodoris_orientalis", "Goniobranchus_coi", 
  "Goniobranchus_fidelis", "Goniobranchus_geometricus", "Goniobranchus_kuniei", 
  "Goniobranchus_leopardus", "Tritonia_tetraquetra", "Dirona_pellucida", "Aplysia_californica", 
  "Arion_vulgaris", "Candidula_unifasciata", "Oreohelix_idahoensis", "Biomphalaria_glabrata", 
  "Biomphalaria_straminea", "Biomphalaria_pfeifferi", "Anisus_vortex", "Lymnaea_stagnalis", 
  "Ampullaceana_balthica", "Radix_auricularia", "Physella_acuta", "Elysia_chlorotica", 
  "Elysia_marginata", "Plakobranchus_ocellatus", "Gibbula_magus", "Phorcus_lineatus", 
  "Haliotis_cracherodii", "Haliotis_laevigata", "Haliotis_discus_hannai", "Haliotis_rufescens", 
  "Dracogyra_subfuscus", "Gigantopelta_aegis", "Chrysomallon_squamiferum", "Patella_depressa", 
  "Patella_pellucida", "Patella_vulgata", "Lottia_gigantea", "Lottia_scabra"
)

# Bivalvia species
target_words <- c(
  "Potamilus_streckersoni", "Cristaria_plicata", "Hyriopsis_cumingii", "Unio_delphinus", 
  "Unio_pictorum", "Margaritifera_margaritifera", "Panopea_generosa", "Mya_arenaria", 
  "Dreissena_polymorpha", "Dreissena_rostriformis", "Congeria_kusceri", "Ruditapes_philippinarum", 
  "Saxidomus_purpurata", "Mercenaria_mercenaria", "Spisula_solida", "Mactra_quadrangularis", 
  "Archivesica_marissinica", "Corbicula_fluminea", "Cerastoderma_edule", "Fragum_fragum", 
  "Fragum_whitleyi", "Hippopus_hippopus", "Tridacna_crocea", "Tridacna_gigas", 
  "Sinonovacula_constricta", "Gari_tellinella", "Conchocele_bisecta", "Mizuhopecten_yessoensis", 
  "Patinopecten_yessoensis", "Pecten_maximus", "Argopecten_purpuratus", "Chlamys_farreri", 
  "Ostrea_edulis", "Ostrea_lurida", "Magallana_hongkongensis", "Crassostrea_gigas", 
  "Crassostrea_virginica", "Crassostrea_angulata", "Crassostrea_ariakensis", 
  "Crassostrea_hongkongensis", "Saccostrea_glomerata", "Atrina_japonica", "Pinctada_imbricata", 
  "Pinctada_fucata", "Pinctada_fucata_martensii", "Scapharca_broughtonii", "Tegillarca_granosa", 
  "Mytilisepta_virgata", "Modiolus_philippinarum", "Bathymodiolus_platifrons", 
  "Mytilus_californianus", "Mytilus_coruscus", "Mytilus_edulis", "Mytilus_galloprovincialis", 
  "Perna_viridis", "Botula_fusca", "Limnoperna_fortunei", "Lithophaga_antillarum", 
  "Lithophaga_nigra", "Laternula_elliptica"
)

# Cephalopoda species
target_words <- c(
  "Nautilus_pompilius", "Argonauta_argo", "Octopus_bimaculoides", "Octopus_minor", 
  "Octopus_sinensis", "Octopus_vulgaris", "Octopus_maya", "Octopus_mimus", 
  "Octopus_insularis", "Octopus_rubescens", "Hapalochlaena_maculosa", 
  "Muusoctopus_leioderma", "Muusoctopus_longibrachus", "Japetella_diaphana", 
  "Sepia_pharaonis", "Euprymna_scolopes", "Octopoteuthis_deletron", 
  "Watasenia_scintillans", "Architeuthis_dux"
)

# Other molluscan classes
target_words <- c(
  "Wirenia_argentea", "Gymnomenia_pellucida", "Epimenia_babai", 
  "Acanthochitona_crinita", "Acanthopleura_granulata", "Mopalia_swanii", 
  "Mopalia_vespertina", "Laevipilina_antarctica", "Gadila_tolmiei"
)

# Annelida species
target_words <- c(
  "Capitella_teleta", "Sthenelais_limicola", "Alitta_virens", "Platynereis_dumerilii", 
  "Lepidonotus_clava", "Harmothoe_impar", "Acholoe_squamosa", "Alentia_gelatinosa", 
  "Amphiduros_pacificus", "Dimorphilus_gyrociliatus", "Riftia_pachyptila", 
  "Lamellibrachia_luymesi", "Lamellibrachia_satsuma", "Paraescarpia_echinospica", 
  "Ridgeia_piscesae", "Owenia_fusiformis", "Hydroides_elegans", "Paralvinella_palmiformis", 
  "Terebella_lapidaria", "Streblospio_benedicti", "Metaphire_vulgaris", "Amynthas_corticis", 
  "Eudrilus_eugeniae", "Aporrectodea_caliginosa", "Lumbricus_terrestris", "Hirudo_medicinalis", 
  "Hirudo_verbana", "Poecilobdella_manillensis", "Hirudinaria_manillensis", "Whitmania_pigra", 
  "Piscicola_geometra", "Helobdella_robusta", "Sipunculus_nudus"
)

# Platyhelminthes
target_words <- c(
  "Dugesia_japonica", "Girardia_tigrina", "Schmidtea_mediterranea", "Macrostomum_lignano",
  "Macrostomum_cliftonense", "Macrostomum_hystrix", "Prostheceraeus_crozeri", "Eudiplozoon_nipponicum",
  "Protopolystoma_xenopodis", "Gyrodactylus_bullatarudis", "Gyrodactylus_salaris", "Atriophallophorus_winterbourni",
  "Echinostoma_caproni", "Fasciola_gigantica", "Fasciola_hepatica", "Fasciolopsis_buski", "Paragonimus_heterotremus",
  "Paragonimus_kellicotti", "Paragonimus_skrjabini_miyazakii", "Paragonimus_westermani", "Dicrocoelium_dendriticum",
  "Opisthorchis_felineus", "Opisthorchis_viverrini", "Clonorchis_sinensis", "Schistosoma_rodhaini",
  "Schistosoma_bovis", "Schistosoma_curassoni", "Schistosoma_haematobium", "Schistosoma_japonicum",
  "Schistosoma_margrebowiei", "Schistosoma_mattheei", "Schistosoma_guineensis", "Schistosoma_intercalatum",
  "Schistosoma_turkestanicum", "Schistosoma_mansoni", "Heterobilharzia_americana", "Trichobilharzia_regenti",
  "Cardicola_forsteri", "Dibothriocephalus_latus", "Schistocephalus_solidus", "Sparganum_proliferum",
  "Spirometra_erinaceieuropaei", "Echinococcus_granulosus", "Echinococcus_canadensis", "Echinococcus_multilocularis",
  "Echinococcus_oligarthrus", "Hydatigera_taeniaeformis", "Taenia_asiatica", "Taenia_solium", "Taenia_saginata",
  "Taenia_crassiceps", "Taenia_multiceps", "Taenia_pisiformis", "Dipylidium_caninum", "Moniezia_expansa",
  "Hymenolepis_diminuta", "Rodentolepis_nana", "Mesocestoides_corti", "Proteocephalus_fallax"
)

# Other lophotrochozoan phyla
target_words <- c(
  "Pedicellina_cernua", "Bugula_neritina", "Membranipora_membranacea", 
  "Terminoflustra_membranaceotruncata", "Cryptosula_pallasiana", "Cristatella_mucedo", 
  "Phoronis_pallida", "Phoronis_australis", "Phoronis_ovalis", "Lingula_anatina", 
  "Lingula_unguis", "Notospermus_geniculatus", "Lineus_longissimus"
)

# Subset the data frame to include only rows for target species
# Assume the original dataset is named 'all_species' with a column 'Species'
data <- all_species[all_species$Species %in% target_words, ]

# Reshape data to long format for use in ggplot
melted_data <- melt(data, id.vars = "Species")

# Clean up species names by removing underscores
melted_data$Species <- gsub("_", " ", melted_data$Species)

# Ensure species are ordered by the target list for consistent plotting
melted_data$Species <- factor(melted_data$Species, levels = rev(gsub("_", " ", target_words)))

# Create the heatmap plot
ggplot(melted_data, aes(x = variable, y = Species, fill = value)) +
  
  # Loop through variables to apply different fill scales for each
  purrr::imap(
    split(melted_data, ~variable),
    \(x, y) {
      list(
        ggnewscale::new_scale_fill(),  # Reset fill scale for each variable
        geom_tile(data = x, aes(fill = value), color = "black"),  # Create heatmap tiles
        scale_fill_gradientn(colors = colors, na.value = "white", name = y, guide = "colorbar")  # Set color scale
      )
    }
  ) +
  
  # Add text labels showing the rounded values on the heatmap
  geom_text(aes(label = round(value, 2)), color = "black") +
  
  # Apply minimal theme for cleaner visual output
  theme_minimal() +
  
  # Customize the appearance of axis text and rotate labels for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # Adjust aspect ratio for heatmap tiles
  coord_fixed(ratio = 1)
