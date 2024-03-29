//
//  Navigate.swift
//  mo-nomz
//
//  Created by Dan Fithian on 2/22/22.
//

import UIKit

extension UIViewController {
    func withMainVc(_ f: ((SceneDelegate, ViewController) -> Void)?) {
        DispatchQueue.main.async {
            let scene = UIApplication.shared.connectedScenes.first?.delegate as! SceneDelegate
            let mainSb = UIStoryboard(name: "Main", bundle: nil)
            let mainVc = mainSb.instantiateInitialViewController() as! ViewController
            f?(scene, mainVc)
        }
    }
    
    func loadGroceries() {
        withMainVc({ scene, mainVc in
            let GROCERY_TAB = 0
            scene.window?.rootViewController = mainVc
            mainVc.setSelected(index: GROCERY_TAB)
        })
    }
    
    func createOrLoadRecipe(_ url: URL) {
        guard let host = url.host else { return }
        if let recipe = Database.findRecipeByLink(host: host, path: url.path) {
            if !recipe.recipe.active {
                Database.updateRecipe(id: recipe.id, recipe: ReadableRecipe(name: recipe.recipe.name, link: recipe.recipe.link, active: true, rating: recipe.recipe.rating, notes: recipe.recipe.notes, ingredients: recipe.recipe.ingredients, steps: recipe.recipe.steps, tags: recipe.recipe.tags))
            }
            loadRecipe(recipe)
        } else {
            createRecipe(url)
        }
    }
    
    func createRecipe(_ url: URL) {
        withMainVc { scene, mainVc in
            let RECIPE_TAB = 1
            scene.window?.rootViewController = mainVc
            mainVc.setSelected(index: RECIPE_TAB)
            mainVc.addLink(link: url.absoluteString, completion: { response in
                let recipe = Database.insertRecipe(response: response, link: url.absoluteString, active: true, tags: [])
                mainVc.reloadData()
                if let vc = mainVc.tab?.viewControllers?[RECIPE_TAB] as? RecipeController {
                    DispatchQueue.main.async {
                        vc.recipeVc?.performSegue(withIdentifier: "showRecipe", sender: recipe)
                    }
                }
            })
        }
    }
    
    func loadRecipe(_ recipe: ReadableRecipeWithId) {
        withMainVc { scene, mainVc in
            let RECIPE_TAB = 1
            scene.window?.rootViewController = mainVc
            mainVc.setSelected(index: RECIPE_TAB)
            if let vc = mainVc.tab?.viewControllers?[RECIPE_TAB] as? RecipeController {
                DispatchQueue.main.async {
                    vc.recipeVc?.performSegue(withIdentifier: "showRecipe", sender: recipe)
                }
            }
        }
    }
}
