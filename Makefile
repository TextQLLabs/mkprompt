pull:
	docker pull ahaym/mkprompt:latest
run: pull
	docker run -it --rm -p 4080:4080 ahaym/mkprompt:latest
deploy:
	sh deploy.sh
