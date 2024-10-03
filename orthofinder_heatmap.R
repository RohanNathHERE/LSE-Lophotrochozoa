# Load required libraries
library(ggplot2)  # For plotting
library(reshape2) # For melting the data

# Melt the data for easier plotting (converts wide format to long format)
data = melt(table.csv)

# Rename columns for better clarity
colnames(data)[1] = "variable1"  # Represents species in the y-axis
colnames(data)[2] = "variable2"  # Represents species in the x-axis

# Replace underscores with spaces in species names for better readability in plots
data$variable1 <- gsub("_", " ", data$variable1)
data$variable2 <- gsub("_", " ", data$variable2)

# Define species order for each taxonomic group (based on phylogeny or desired order)
# Annelida species order
species_order <- c(
  "Capitella_teleta", "Sthenelais_limicola", "Alitta_virens", "Platynereis_dumerilii", 
  "Lepidonotus_clava", "Harmothoe_impar", "Acholoe_squamosa", "Alentia_gelatinosa", 
  "Amphiduros_pacificus", "Dimorphilus_gyrociliatus", "Riftia_pachyptila", 
  "Lamellibrachia_luymesi", "Lamellibrachia_satsuma", "Paraescarpia_echinospica", 
  "Ridgeia_piscesae", "Hydroides_elegans", "Streblospio_benedicti", 
  "Paralvinella_palmiformis", "Terebella_lapidaria", "Metaphire_vulgaris", 
  "Amynthas_corticis", "Eudrilus_eugeniae", "Aporrectodea_caliginosa", 
  "Lumbricus_terrestris", "Hirudo_medicinalis", "Hirudo_verbana", 
  "Poecilobdella_manillensis", "Hirudinaria_manillensis", "Whitmania_pigra", 
  "Piscicola_geometra", "Helobdella_robusta", "Sipunculus_nudus", 
  "Owenia_fusiformis"
)

# Bivalvia species order
species_order <- c(
  "Potamilus_streckersoni", "Cristaria_plicata", "Hyriopsis_cumingii", "Unio_delphinus", 
  "Unio_pictorum", "Margaritifera_margaritifera", "Panopea_generosa", "Mya_arenaria", 
  "Dreissena_polymorpha", "Dreissena_rostriformis", "Congeria_kusceri", "Ruditapes_philippinarum", 
  "Saxidomus_purpurata", "Mercenaria_mercenaria", "Spisula_solida", 
  "Mactra_quadrangularis", "Archivesica_marissinica", "Corbicula_fluminea", 
  "Cerastoderma_edule", "Fragum_fragum", "Fragum_whitleyi", "Hippopus_hippopus", 
  "Tridacna_crocea", "Tridacna_gigas", "Sinonovacula_constricta", 
  "Gari_tellinella", "Conchocele_bisecta", "Mizuhopecten_yessoensis", 
  "Patinopecten_yessoensis", "Pecten_maximus", "Argopecten_purpuratus", 
  "Chlamys_farreri", "Ostrea_edulis", "Ostrea_lurida", 
  "Magallana_hongkongensis", "Crassostrea_gigas", "Crassostrea_virginica", 
  "Crassostrea_angulata", "Crassostrea_ariakensis", "Crassostrea_hongkongensis", 
  "Saccostrea_glomerata", "Atrina_japonica", "Pinctada_imbricata", 
  "Pinctada_fucata", "Pinctada_fucata_martensii", "Scapharca_broughtonii", 
  "Tegillarca_granosa", "Mytilisepta_virgata", "Modiolus_philippinarum", 
  "Bathymodiolus_platifrons", "Mytilus_californianus", "Mytilus_coruscus", 
  "Mytilus_edulis", "Mytilus_galloprovincialis", "Perna_viridis", 
  "Botula_fusca", "Limnoperna_fortunei", "Lithophaga_antillarum", 
  "Lithophaga_nigra", "Laternula_elliptica"
)

# Cephalopoda species order
species_order <- c(
  "Nautilus_pompilius", "Argonauta_argo", "Octopus_bimaculoides", 
  "Octopus_minor", "Octopus_sinensis", "Octopus_vulgaris", 
  "Octopus_maya", "Octopus_mimus", "Octopus_insularis", 
  "Octopus_rubescens", "Hapalochlaena_maculosa", "Muusoctopus_leioderma", 
  "Muusoctopus_longibrachus", "Japetella_diaphana", "Sepia_pharaonis", 
  "Euprymna_scolopes", "Octopoteuthis_deletron", "Watasenia_scintillans", 
  "Architeuthis_dux"
)

# Gastropoda species order
species_order <- c(
  "Alviniconcha_marisindica", "Batillaria_attramentaria", 
  "Melanoides_tuberculata", "Babylonia_areolata", "Conus_betulinus", 
  "Conus_consors", "Conus_tribblei", "Rapana_venosa", "Pomacea_canaliculata", 
  "Chromodoris_orientalis", "Goniobranchus_coi", "Goniobranchus_fidelis", 
  "Goniobranchus_geometricus", "Goniobranchus_kuniei", 
  "Goniobranchus_leopardus", "Tritonia_tetraquetra", "Dirona_pellucida", 
  "Aplysia_californica", "Arion_vulgaris", "Candidula_unifasciata", 
  "Oreohelix_idahoensis", "Biomphalaria_glabrata", "Biomphalaria_straminea", 
  "Biomphalaria_pfeifferi", "Anisus_vortex", "Lymnaea_stagnalis", 
  "Ampullaceana_balthica", "Radix_auricularia", "Physella_acuta", 
  "Elysia_chlorotica", "Elysia_marginata", "Plakobranchus_ocellatus", 
  "Gibbula_magus", "Phorcus_lineatus", "Haliotis_cracherodii", 
  "Haliotis_laevigata", "Haliotis_discus_hannai", "Haliotis_rufescens", 
  "Dracogyra_subfuscus", "Gigantopelta_aegis", "Chrysomallon_squamiferum", 
  "Patella_depressa", "Patella_pellucida", "Patella_vulgata", 
  "Lottia_gigantea", "Lottia_scabra"
)

# Other lophotrochozoan phyla
species_order <- c(
  "Pedicellina_cernua", "Bugula_neritina", "Membranipora_membranacea", 
  "Terminoflustra_membranaceotruncata", "Cryptosula_pallasiana", "Cristatella_mucedo", 
  "Phoronis_pallida", "Phoronis_australis", "Phoronis_ovalis", "Lingula_anatina", 
  "Lingula_unguis", "Notospermus_geniculatus", "Lineus_longissimus"
)

# Other molluscan classes
species_order <- c(
  "Wirenia_argentea", "Gymnomenia_pellucida", "Epimenia_babai", 
  "Acanthochitona_crinita", "Acanthopleura_granulata", "Mopalia_swanii", 
  "Mopalia_vespertina", "Laevipilina_antarctica", "Gadila_tolmiei"
  
)
# Platyhelminthes
species_order <- c(
  "Dugesia_japonica", "Girardia_tigrina", "Schmidtea_mediterranea", "Macrostomum_lignano",
  "Macrostomum_cliftonense", "Macrostomum_hystrix", "Prostheceraeus_crozieri", "Eudiplozoon_nipponicum",
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

# Remove underscores for better display
species_order <- gsub("_", " ", species_order)

# Reverse the factor levels for variable1 (species on y-axis)
data$variable1 <- factor(data$variable1, levels = rev(species_order))

# Set the levels for variable2 (species on x-axis)
data$variable2 <- factor(data$variable2, levels = species_order)

# Create the heatmap using ggplot2
ggplot(data, aes(x = variable2, y = variable1, fill = value)) +
  geom_tile(color = "white", size = 0.45) +  # Create tiles for each cell with borders
  # Optional: Uncomment the line below if you want to add text labels to non-zero cells
  # geom_text(aes(label = ifelse(value != 0, value, "")), size = 3) +
  
  # Define color gradient and breaks for the fill (values)
  scale_fill_gradientn(
    colors = c("#f9eff0", "#5b8dce", "#8bd0df", "#92dfdc", "#a8ecd2", "#c9f6c7", "#d7f2b6", 
               "#e9eca7", "#fce59c", "#fcd185"),
    breaks = c(0, 1, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 90),
    limits = c(min(data$value), max(data$value))
  ) +
  
  # Add titles and axis labels
  labs(
    title = "Genes Forming One-to-One Orthologues",
    x = "", y = ""
  ) +
  
  # Adjust text and appearance of the plot
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(size = 13, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7)
  )
