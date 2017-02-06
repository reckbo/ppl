.PHONY: all
all: pip_packages.nix

pip_packages.nix: requirements.txt
	if [ ! -d "pip2nix" ]; then \
		git clone https://github.com/acowley/pip2nix; \
  fi
	cd pip2nix; nix-shell --run 'pip2nix ../requirements.txt -o ../pip_packages.nix'
	@echo "Now run 'nix-shell'"

conda: requirements.txt
	conda env create -f requirements.txt -n ppl

env: requirements.txt
	virtualenv env; env/bin/pip install -r requirements.txt
