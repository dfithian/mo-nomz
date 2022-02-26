//
//  Navigate.swift
//  mo-nomz
//
//  Created by Dan Fithian on 2/22/22.
//

import UIKit

extension UIViewController {
    func withMainVc(_ f: ((SceneDelegate, UITabBarController) -> Void)?) {
        let scene = UIApplication.shared.connectedScenes.first?.delegate as! SceneDelegate
        let mainSb = UIStoryboard(name: "Main", bundle: nil)
        let mainVc = mainSb.instantiateInitialViewController() as! UITabBarController
        f?(scene, mainVc)
    }
    
    func loadGroceries() {
        withMainVc { scene, mainVc in
            DispatchQueue.main.async {
                scene.window?.rootViewController = mainVc
            }
        }
    }
    
    func createOrLoadRecipe(_ url: URL) {
        guard let host = url.host else { return }
        if let recipe = Database.findRecipeByLink(host: host, path: url.path) {
            if !recipe.recipe.active {
                Database.updateRecipe(id: recipe.id, recipe: ReadableRecipe(name: recipe.recipe.name, link: recipe.recipe.link, active: true, rating: recipe.recipe.rating, notes: recipe.recipe.notes, ingredients: recipe.recipe.ingredients, steps: recipe.recipe.steps  ))
            }
            loadRecipe(recipe)
        } else {
            createRecipe(url)
        }
    }
    
    func createRecipe(_ url: URL) {
        withMainVc { scene, mainVc in
            let RECIPE_TAB = 1
            DispatchQueue.main.async {
                scene.window?.rootViewController = mainVc
                mainVc.selectedIndex = RECIPE_TAB
                let vc = mainVc.viewControllers![RECIPE_TAB] as! RecipeController
                vc.recipeVc?.addLink(link: url.absoluteString, active: true, completion: { recipe in
                    vc.loadData()
                    DispatchQueue.main.async {
                        vc.recipeVc?.performSegue(withIdentifier: "showRecipe", sender: recipe)
                    }
                })
            }
        }
    }
    
    func loadRecipe(_ recipe: ReadableRecipeWithId) {
        withMainVc { scene, mainVc in
            let RECIPE_TAB = 1
            DispatchQueue.main.async {
                scene.window?.rootViewController = mainVc
                mainVc.selectedIndex = RECIPE_TAB
                let vc = mainVc.viewControllers![RECIPE_TAB] as! RecipeController
                vc.recipeVc?.performSegue(withIdentifier: "showRecipe", sender: recipe)
            }
        }
    }
}