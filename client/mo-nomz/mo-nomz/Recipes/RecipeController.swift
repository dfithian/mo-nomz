//
//  RecipeController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import UIKit

class RecipeController: UIViewController {
    var recipeVc: RecipeListController? = nil

    private func loadRecipes() {
        let completion = { [weak self] (resp: ListRecipeResponse) -> Void in
            let recipes = resp.recipes.map({
                RecipeWithId(recipe: $0.value, id: $0.key)
            })
            self?.recipeVc?.active = recipes.filter({ $0.recipe.active })
            self?.recipeVc?.saved = recipes.filter({ !$0.recipe.active })
            DispatchQueue.main.async {
                self?.recipeVc?.table.reloadData()
            }
        }
        Actions.loadRecipes(completion: completion, onError: self.defaultOnError)
    }

    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? RecipeAddLinkController, segue.identifier == "addLink" {
            vc.onChange = { () -> Void in
                self.loadRecipes()
            }
        }
        if let vc = segue.destination as? RecipeListController, segue.identifier == "embedRecipes" {
            recipeVc = vc
            loadRecipes()
            vc.onChange = { () -> Void in
                self.loadRecipes()
            }
        }
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadRecipes()
    }
}
