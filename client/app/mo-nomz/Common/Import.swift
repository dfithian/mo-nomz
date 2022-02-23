//
//  Import.swift
//  mo-nomz
//
//  Created by Dan Fithian on 2/22/22.
//

import UIKit

extension UIViewController {
    func importRecipe(_ url: URL) -> Bool {
        let RECIPE_TAB = 1
        let scene = UIApplication.shared.connectedScenes.first?.delegate as! SceneDelegate
        let mainSb = UIStoryboard(name: "Main", bundle: nil)
        let mainVc = mainSb.instantiateInitialViewController() as! UITabBarController
        guard let host = url.host else { return false }
        let recipeMay = Database.findRecipeByLink(host: host, path: url.path)
        DispatchQueue.main.async {
            scene.window?.rootViewController = mainVc
            mainVc.selectedIndex = RECIPE_TAB
            let vc = mainVc.viewControllers![RECIPE_TAB] as! RecipeController
            if let recipe = recipeMay {
                vc.recipeVc?.performSegue(withIdentifier: "showRecipe", sender: recipe)
            } else {
                vc.recipeVc?.addLink(link: url.absoluteString, active: true, completion: { recipe in
                    vc.loadData()
                    DispatchQueue.main.async {
                        vc.recipeVc?.performSegue(withIdentifier: "showRecipe", sender: recipe)
                    }
                })
            }
        }
        return true
    }
}
