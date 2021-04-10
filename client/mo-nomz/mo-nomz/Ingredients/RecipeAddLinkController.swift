//
//  RecipeAddLinkController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit

class RecipeAddLinkController: UIViewController {
    @IBOutlet weak var link: UITextField!
    var onChange: (() -> Void)?

    @IBAction func didTapSave(_ sender: Any) {
        Actions.addRecipeLink(link: link.text!, completion: { () -> Void in
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.onChange?()
        })
    }
    
    @IBAction func didTapCancel(_ sender: Any) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        link.becomeFirstResponder()
    }
}
