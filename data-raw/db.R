db <- DBI::dbConnect(RSQLite::SQLite(), "data-raw/chinook.sqlite")
c("Album", "Artist", "Customer", "Employee", "Genre", "Invoice",
  "InvoiceLine", "MediaType", "Playlist", "PlaylistTrack", "Track"
)
