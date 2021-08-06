open Env
module R =
struct
  (* Création des opérations de manipulation des relations à valeur dans Value *)
  include Relation.Make(Value)
end
   let _ =
      let vin_att,vin = R.from_file "vin.csv" '|' in
      let viticulteur_att, viticulteur = R.from_file "viticulteur.csv" '|' in
      let client_att, client = R.from_file "client.csv" '|' in
      let commande_att, commande = R.from_file "commande.csv" '|' in
      let env=empty in
      let env = (add "vin" (vin,(vin_att)) env) in
      let env = (add "viticulteur" (viticulteur,(viticulteur_att)) env) in
      let env = (add "client" (client,(client_att)) env) in
      let env = (add "commande" (commande,(commande_att)) env) in
      (* Ouverture un flot de caractère ; ici à partir de l'entrée standard *)
      let source = Lexing.from_channel stdin in
      (* Boucle infinie interompue par une exception correspondant à la fin de fichier *)
      let rec f () =
        try
          (* Récupération d'une expression à partir de la source puis affichage de l'évaluation *)
          let e = Parser.ansyn Lexer.anlex source in
           Printf.printf "=>%s\n" (Ast.pp_query e); (R.print '|' [] (Ast.eval_query env e)); Printf.printf "\n"; flush stdout;
          f ()
        with Lexer.Eof -> Printf.printf "Bye\n"
      in
      f ()