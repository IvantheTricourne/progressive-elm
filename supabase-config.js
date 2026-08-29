// Supabase connection settings.
//
// Both values are safe to commit and to serve publicly — the anon key is
// designed to ship in client code. What protects the data is row-level
// security in supabase/migrations/, which scopes every row to auth.uid().
// Never put the service_role key here.
//
// Left as placeholders, the app runs entirely on localStorage and hides the
// sync controls, so it still works as a standalone demo.
window.SUPABASE_CONFIG = {
  url: "https://YOUR_PROJECT.supabase.co",
  anonKey: "YOUR_ANON_KEY",
};
