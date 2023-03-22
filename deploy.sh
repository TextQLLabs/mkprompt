nix build ".#dockerImage" --json | jq -r .[0]."outputs"."out" | xargs sudo docker load -i | sed "s/Loaded image: \(.*\)/\1/g" | xargs -I{} sudo docker tag {} ahaym/mkprompt:latest

sudo docker push ahaym/mkprompt:latest

