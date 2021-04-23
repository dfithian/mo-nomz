//
//  RecipeAddController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit

class RecipeAddController: UIViewController {
    @IBOutlet weak var link: UITextField!
    var onChange: (() -> Void)?

    @IBAction func didTapSave(_ sender: Any) {
        let completion = { () -> Void in
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.onChange?()
        }
        if let newLink = link.text, !newLink.isEmpty {
            addRecipeLink(link: link.text!, completion: completion)
        } else {
            didTapCancel(sender)
        }
    }
    
    @IBAction func didTapCancel(_ sender: Any) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        link.becomeFirstResponder()
        link.addDoneButtonOnKeyboard()
    }
}
