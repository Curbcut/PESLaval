c("Maison des arts de Laval (théâtre des Muses)",

"Salle Alfred-Pellan",

"Studio (Maison des arts)",

"Arts de la scène",
"Théâtre du Bout de l’île",
"Théâtre de la Grangerit",
"Théâtre du Vieux-Saint-Vincent",

"Centre d’art de Sainte-Rose",
"Galerie d’art de la Vieille Caserne ",
"Hall de l’hôtel de ville",
"Hall du centre communautaire Accès",

"Atelier 213 – arts visuels, métiers d’art",
"Pavillon Argenteuil – arts de la scène",
"Pavillon Bon-Pasteur – arts de la scène, métiers d’art",
"Place des aînés – arts de la scène, arts visuels",
"Centre d’art de Sainte-Rose – arts visuels",

"Espace Michelin",

"Maison André-Benjamin-Papineau",
"Centre Alain-Grandbois",

"Bibliothèque Émile-Nelligan (Laval-des-Rapides)",
"Bibliothèque Gabrielle-Roy (Fabreville)",
"Bibliothèque Germaine-Guèvremont (Duvernay)",
"Bibliothèque Laure-Conan (Auteuil)",
"Bibliothèque Marius-Barbeau (Saint-François)",
"Bibliothèque multiculturelle (Chomedey)",
"Bibliothèque Philippe-Panneton (Laval-Ouest)",
"Bibliothèque Sylvain-Garneau (Sainte-Rose)",
"Bibliothèque Yves-Thériault (Sainte-Dorothée)",

"Berge des Baigneurs",
"Berge aux Quatre-Vents",
"Centre de la nature",
"Centre-ville, secteur Montmorency",
"Parc des Prairies" ,
"Place publique Sainte-Dorothée",
"Saint-François",
"Vieux-Sainte-Rose",
"Parcs famille",

"Observatoire du Centre de la nature",

"Duplex André-Benjamin-Papineau",
"Centre arménien",


"Arts de la scène",
"Salle André-Mathieu",
"Théâtre Marcellin-Champagnat (Collège Laval)",

"Chapelle du Mont-de-La Salle",
"Église de Saint-Elzéar",
"Église de Sainte-Rose-de-Lima",
"Église de Saint-Pie-X",
"Salle Claude Legault du collège Montmorency",
"Salle Claude-Potvin (école Curé-Antoine-Labelle)",
"Salle multifonctionnelle du collège Letendre",

"Hall de la salle André-Mathieu",
"Hall du théâtre Marcellin-Champagnat",

"Camp spatial Canada Cosmodôme" ,
"Musée Armand-Frappier, Centre d’interprétation des biosciences",
"Centre d’interprétation de l’eau" ,
"Parc de la Rivière-des-Mille-Îles",

"Ciné-club de Laval",
"Cineplex de Laval",
"Cinéma Guzzo",

"Galerie le Pépin d’art",
"Galerie d’art Saint-Vincent-de-Paul",

"Bistro le Rossignol",
"Cabaret Apollon",
"Café le Signet",
"Le Balthazar",
"Maison du jazz") -> culturel


culturel <- tibble::tibble(culturel)

culturel_locations <- lapply(paste0(culturel$culturel, ", Laval, QC"), \(x) {
  print(x)
  point <- cc.data::geocode_localhost(x)
  if (!sf::st_is_empty(point)) return(point)
  address_ai <- .chatgpt(sprintf("Return JUST and ONLY the full address for %s", x), write_lines = FALSE)
  address_ai <- gsub("QC.*", "QC", address_ai)
  address_ai <- gsub(" E,", " Est,", address_ai)
  address_ai <- gsub(" O,", " Ouest,", address_ai)
  address_ai <- gsub(" N,", " Nord,", address_ai)
  address_ai <- gsub(" S,", " Sud,", address_ai)
  cc.data::geocode_localhost(address_ai)
})

culturel_locations <- Reduce(rbind, 
       lapply(culturel_locations, \(x) {
         tibble::tibble(geometry = sf::st_sfc(x))
       })
) |> sf::st_as_sf(crs = 4326)

culturel$geometry <- culturel_locations$geometry
culturel <- sf::st_as_sf(culturel, crs = 4326)
culturel <- sf::st_make_valid(culturel)

# Remove empty features
culturel <- culturel[!sf::st_is_empty(culturel), ]

# Remove features outside Mtl
CSD <- cancensus::get_census("CA21", regions = list(CSD = 2465005), level = "CSD",
                                 geo_format = "sf")

# Keep only the ones in the CMA
culturel <- sf::st_transform(culturel, crs = sf::st_crs(CSD))
culturel <- sf::st_filter(culturel, CSD)

qs::qsave(culturel, "data/axe3/locations/culturels_from_diagnostic.qs")

