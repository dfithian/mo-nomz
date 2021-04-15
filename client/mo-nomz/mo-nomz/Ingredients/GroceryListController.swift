//
//  GroceryListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import UIKit

class GroceryListController: UIViewController {
    var ingredientVc: IngredientListController? = nil
    
    @IBAction func export(_ sender: Any?) {
        let ingredients: [ReadableIngredientAggregate] = ((ingredientVc?.ingredients ?? []) + (ingredientVc?.bought ?? []))
        let exportText: String = ingredients.map({ (x: ReadableIngredientAggregate) -> String in
            return "\(x.ingredient.quantity.render()) \(x.ingredient.unit) \(x.ingredient.name)"
        }).joined(separator: "\n")
        let vc = UIActivityViewController(activityItems: [exportText], applicationActivities: nil)
        present(vc, animated: true, completion: nil)
    }
    
    private func loadIngredients() {
        let completion = { [weak self] (resp: ListIngredientResponse) -> Void in
            let ingredients = resp.ingredients
            self?.ingredientVc?.ingredients = ingredients.filter({ $0.ingredient.active })
            self?.ingredientVc?.bought = ingredients.filter({ !$0.ingredient.active })
            DispatchQueue.main.async {
                self?.ingredientVc?.table.reloadData()
            }
        }
        loadIngredients(completion: completion)
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? RecipeAddController, segue.identifier == "addRecipe" {
            vc.onChange = { () -> Void in
                self.loadIngredients()
            }
        }
        if let vc = segue.destination as? IngredientListController, segue.identifier == "embedIngredients" {
            ingredientVc = vc
            loadIngredients()
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
