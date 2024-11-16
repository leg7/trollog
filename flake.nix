{
  description = "env for trollog";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";

  outputs = { nixpkgs, ... }:
  let system = "x86_64-linux";
  in
  {
    devShells."${system}".default =
    let pkgs = import nixpkgs { inherit system; };
    in pkgs.mkShell
    {
      packages = with pkgs; [
        gcc
        ghc
      ];
    };
  };
}
