//
//  GroceryAddBlobController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/23/21.
//

import UIKit

class GroceryAddBlobController: UIViewController {
    @IBOutlet weak var name: UITextField!
    @IBOutlet weak var link: UITextField!
    @IBOutlet weak var blob: UITextView!
    @IBOutlet weak var checkbox: UIButton!
    @IBOutlet weak var noRecipeConstraint: NSLayoutConstraint!
    @IBOutlet weak var recipeConstraint: NSLayoutConstraint!
    var isRecipe: Bool = false
    
    @IBAction func didTapIsRecipe(_ sender: Any?) {
        if isRecipe {
            isRecipe = false
            checkbox.setImage(UIImage(systemName: "square"), for: .normal)
            name.alpha = 0
            link.alpha = 0
            recipeConstraint.isActive = false
            noRecipeConstraint.isActive = true
            self.view.layoutIfNeeded()
        } else {
            isRecipe = true
            checkbox.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
            name.alpha = 1
            link.alpha = 1
            noRecipeConstraint.isActive = false
            recipeConstraint.isActive = true
            self.view.layoutIfNeeded()
        }
    }
    
    func save(_ onChange: (() -> Void)?, onCancel: (() -> Void)?) {
        if let content = blob.text, !content.isEmpty {
            let completion = {
                DispatchQueue.main.async {
                    self.dismiss(animated: true, completion: nil)
                }
                onChange?()
            }
            if isRecipe {
                if let name = name.text, name != "" {
                    let link = (link.text != nil && link.text != "" ? link.text : nil)
                    addGroceryBlob(name: name, link: link, content: content, completion: completion)
                } else {
                    alertUnsuccessful("To save as a recipe, please provide a name.")
                }
            } else {
                addGroceryBlob(name: nil, link: nil, content: content, completion: completion)
            }
        } else {
            onCancel?()
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        name.addDoneButtonOnKeyboard()
        name.alpha = 0
        link.addDoneButtonOnKeyboard()
        link.alpha = 0
        blob.addDoneButtonOnKeyboard()
        blob.layer.cornerRadius = 10
    }
}
