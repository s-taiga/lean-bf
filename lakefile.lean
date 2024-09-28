import Lake
open Lake DSL

package «bf» where
  -- add package configuration options here

lean_lib «Bf» where
  -- add library configuration options here

require mathlib from git "https://github.com/leanprover-community/mathlib4"@"v4.11.0"

@[default_target]
lean_exe «bf» where
  root := `Main
