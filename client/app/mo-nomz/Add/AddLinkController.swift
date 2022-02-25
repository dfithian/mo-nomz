//
//  AddLinkController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit
import SafariServices

class AddLinkController: AddDetailController {
    @IBOutlet weak var link: UITextField!
    @IBOutlet weak var checkbox: UIButton!

    var active: Bool = true
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        let recipeCompletion = { (recipe: ReadableRecipeWithId) in
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.navigationVc?.onChange?()
            self.loadRecipe(recipe)
        }
        let groceryCompletion = {
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.navigationVc?.onChange?()
            self.loadGroceries()
        }
        if let text = link.text?.nonEmpty() {
            if var url = URL(string: text), UIApplication.shared.canOpenURL(url) {
                if let recipeUrl = url.toRecipeUrl() {
                    url = recipeUrl
                }
                if let host = url.host, let recipe = Database.findRecipeByLink(host: host, path: url.path) {
                    if recipe.recipe.active != active {
                        Database.updateRecipe(id: recipe.id, recipe: ReadableRecipe(name: recipe.recipe.name, link: recipe.recipe.link, active: active, rating: recipe.recipe.rating, notes: recipe.recipe.notes, ingredients: recipe.recipe.ingredients, steps: recipe.recipe.steps))
                    }
                    loadRecipe(recipe)
                } else {
                    addLink(link: url.absoluteString, active: active, completion: recipeCompletion)
                }
            } else {
                promptForConfirmationThree(
                    title: "Link to recipe required",
                    message: "That doesn't look like a recipe link. Would you like to add it to your grocery list?\n\nYou can do this under \"Try another way\" > \"Add manual\".",
                    option1Button: "Add as recipe",
                    option1Handler: { _ in self.addLink(link: text, active: self.active, completion: recipeCompletion) },
                    option2Button: "Add to grocery list",
                    option2Handler: { _ in self.addBlob(content: text, completion: { _ in groceryCompletion() }) }
                )
            }
        } else {
            alertUnsuccessful("Please provide a link to a recipe.")
        }
    }
    
    @IBAction func didTapIsActive(_ sender: Any?) {
        if active {
            active = false
            checkbox.setImage(UIImage(systemName: "square"), for: .normal)
        } else {
            active = true
            checkbox.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
        }
    }
    
    override func addType() -> AddType {
        return .link
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        link.addDoneButtonOnKeyboard()
    }
}
