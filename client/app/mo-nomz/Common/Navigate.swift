//
//  Navigate.swift
//  mo-nomz
//
//  Created by Dan Fithian on 2/22/22.
//

import UIKit

protocol MainViewController: UIViewController {
    func mainVcShowGroceries() -> Void
    func mainVcShowRecipe(r: ReadableRecipeWithId) -> Void
}

extension UITabBarController: MainViewController {
    func mainVcShowGroceries() {
        DispatchQueue.main.async {
            self.selectedIndex = 0
        }
    }
    
    func mainVcShowRecipe(r: ReadableRecipeWithId) {
        let recipe = self.viewControllers![1] as! RecipeController
        recipe.reloadData()
        DispatchQueue.main.async {
            self.selectedIndex = 1
            recipe.recipeVc?.performSegue(withIdentifier: "showRecipe", sender: r)
        }
    }
}

extension RecipeController: MainViewController {
    func mainVcShowGroceries() {
        DispatchQueue.main.async {
            self.performSegue(withIdentifier: "showGroceries", sender: nil)
        }
    }
    
    func mainVcShowRecipe(r: ReadableRecipeWithId) {
        self.reloadData()
        DispatchQueue.main.async {
            self.recipeVc?.performSegue(withIdentifier: "showRecipe", sender: r)
        }
    }
}

extension UIViewController {
    func withMainVc(_ f: ((SceneDelegate, MainViewController) -> Void)?) {
        DispatchQueue.main.async {
            let scene = UIApplication.shared.connectedScenes.first?.delegate as! SceneDelegate
            let mainSb = UIStoryboard(name: "Main", bundle: nil)
            let mainVc: MainViewController
            if User.useClassicView() {
                mainVc = mainSb.instantiateViewController(withIdentifier: "tab") as! UITabBarController
            } else {
                mainVc = mainSb.instantiateViewController(withIdentifier: "recipe") as! RecipeController
            }
            f?(scene, mainVc)
        }
    }
    
    func loadGroceries() {
        withMainVc({ scene, mainVc in
            scene.window?.rootViewController = mainVc
            mainVc.mainVcShowGroceries()
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
            scene.window?.rootViewController = mainVc
            mainVc.addLink(link: url.absoluteString, completion: { response in
                let recipe = Database.insertRecipe(response: response, link: url.absoluteString, active: true, tags: [])
                DispatchQueue.main.async {
                    mainVc.mainVcShowRecipe(r: recipe)
                }
            })
        }
    }
    
    func loadRecipe(_ recipe: ReadableRecipeWithId) {
        withMainVc { scene, mainVc in
            scene.window?.rootViewController = mainVc
            mainVc.mainVcShowRecipe(r: recipe)
        }
    }
}
