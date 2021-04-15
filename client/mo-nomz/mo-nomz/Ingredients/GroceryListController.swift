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
        var allIngredients: [IngredientWithStartingIndex] = ((ingredientVc?.ingredients ?? []) + (ingredientVc?.bought ?? []))
        allIngredients.sort()
        let exportText: [String] = allIngredients.map({ (x: IngredientWithStartingIndex) -> String in
            let y = x.ingredient.ingredient
            return "\(y.quantity.render()) \(y.unit) \(y.name)"
        })
        let vc = UIActivityViewController(activityItems: exportText, applicationActivities: nil)
        present(vc, animated: true, completion: nil)
    }
    
    private func loadIngredients() {
        let completion = { [weak self] (resp: ListIngredientResponse) -> Void in
            self?.ingredientVc?.ingredients = zip(resp.ingredients, (0...resp.ingredients.count)).map {
                IngredientWithStartingIndex(ingredient: $0.0, startingIndex: $0.1)
            }
            self?.ingredientVc?.bought = []
            DispatchQueue.main.async {
                self?.ingredientVc?.table.reloadData()
            }
        }
        Actions.loadIngredients(completion: completion, onError: self.defaultOnError)
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? RecipeAddLinkController, segue.identifier == "addLink" {
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
