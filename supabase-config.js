// Supabase connection settings.
//
// Both values are safe to commit and to serve publicly. The publishable key
// (formerly "anon public") is designed to ship in client code; what protects
// the data is row-level security in supabase/migrations/, which scopes every
// row to auth.uid() and grants the anon role nothing.
//
// Never put the secret key (formerly "service_role", sb_secret_...) here.
//
// Cleared back to placeholders, the app runs entirely on localStorage and hides
// the sync controls, so it still works as a standalone demo.
window.SUPABASE_CONFIG = {
  url: "https://agazepvdfkvtssdgvuje.supabase.co",
  anonKey: "sb_publishable_BAVMcbfxRijIJNgl-fvZPQ_SEkUxtAV",
};
