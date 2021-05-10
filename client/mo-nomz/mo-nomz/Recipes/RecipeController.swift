//
//  RecipeController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import UIKit

class RecipeController: UIViewController {
    @IBOutlet weak var banner: UIView!

    var recipeVc: RecipeListController? = nil

    @objc func loadData() {
        let completion = { [weak self] (resp: ListRecipeResponse) -> Void in
            let recipes = resp.recipes.map({
                RecipeWithId(recipe: $0.value, id: $0.key)
            })
            self?.recipeVc?.active = recipes.filter({ $0.recipe.active }).sorted(by: { $0.id < $1.id })
            self?.recipeVc?.saved = recipes.filter({ !$0.recipe.active }).sorted(by: { $0.id < $1.id })
            DispatchQueue.main.async {
                self?.recipeVc?.tableView.reloadData()
            }
        }
        loadRecipes(completion: completion)
    }

    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? RecipeAddController, segue.identifier == "addLink" {
            vc.onChange = { () -> Void in
                self.loadData()
            }
        }
        if let vc = segue.destination as? GroceryAddController, segue.identifier == "addGroceries" {
            vc.onChange = { () -> Void in
                self.loadData()
            }
        }
        if let vc = segue.destination as? RecipeListController, segue.identifier == "embedRecipes" {
            recipeVc = vc
            loadData()
            vc.onChange = { () -> Void in
                self.loadData()
            }
        }
        if let vc = segue.destination as? BannerController, segue.identifier == "embedBanner" {
            vc.height = banner.constraints.filter({ $0.identifier == "height" }).first
        }
    }
    
    override func viewDidLoad() {
        NotificationCenter.default.addObserver(self, selector: #selector(loadData), name: UIApplication.willEnterForegroundNotification, object: nil)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadData()
    }
}
