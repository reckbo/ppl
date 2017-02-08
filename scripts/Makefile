.PHONY: all nix

nix: pip_packages.nix

pip_packages.nix: requirements.txt
	if [ ! -d "_pip2nix" ]; then \
		git clone https://github.com/acowley/pip2nix _pip2nix; \
  fi
	# cp $< /tmp;
	# echo ipython >> /tmp/requirements.txt;
	# cd _pip2nix; nix-shell --run 'pip2nix /tmp/requirements.txt -o ../pip_packages.nix'
	cd _pip2nix; nix-shell --run 'pip2nix ../requirements.txt -o ../pip_packages.nix'
	@echo "Now run 'nix-shell'"

conda: requirements.txt
	conda env create -f requirements.txt -n ppl
	@echo "Now run `source activate ppl`"

env: requirements.txt
	virtualenv env; env/bin/pip install -r requirements.txt
	@echo "Now run `source env/bin/activate ppl`"
