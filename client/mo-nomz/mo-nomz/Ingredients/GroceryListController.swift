//
//  GroceryListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import UIKit

class GroceryListController: UIViewController {
    var ingredientVc: IngredientListController? = nil
    
    private func loadIngredients() {
        Actions.loadIngredients(completion: { [weak self] (resp) -> Void in
            self?.ingredientVc?.ingredients = zip(resp.ingredients, (0...resp.ingredients.count)).map {
                IngredientWithStartingIndex(ingredient: $0.0, startingIndex: $0.1)
            }
            DispatchQueue.main.async {
                self?.ingredientVc?.table.reloadData()
            }
        })
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? IngredientListController, segue.identifier == "embedIngredients" {
            ingredientVc = vc
            loadIngredients()
            vc.onChange = { () -> Void in
                self.loadIngredients()
            }
        }
        if let vc = segue.destination as? RecipeAddLinkController, segue.identifier == "addLink" {
            vc.onChange = { () -> Void in
                self.loadIngredients()
            }
        }
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadIngredients()
    }
}
