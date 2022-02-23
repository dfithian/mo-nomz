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
        if let newLink = link.text?.nonEmpty(), var url = URL(string: newLink) {
            if let import_ = RecipeImport.parse(url) {
                url = import_.url
            }
            if let host = url.host, let recipe = Database.findRecipeByLink(host: host, path: url.path) {
                loadRecipe(recipe)
            } else {
                addLink(link: url.absoluteString, active: active, completion: recipeCompletion)
            }
        } else {
            alertUnsuccessful("Please provide a link.")
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
