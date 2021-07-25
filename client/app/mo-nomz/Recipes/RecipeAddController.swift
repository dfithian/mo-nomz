//
//  RecipeAddController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit
import SafariServices

class RecipeAddController: UIViewController {
    @IBOutlet weak var link: UITextField!
    @IBOutlet weak var checkbox: UIButton!
    var existingLinks: [String] = []
    var active: Bool = true
    
    @IBAction func didTapIsActive(_ sender: Any?) {
        if active {
            active = false
            checkbox.setImage(UIImage(systemName: "square"), for: .normal)
        } else {
            active = true
            checkbox.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
        }
    }

    func save(_ onChange: (() -> Void)?, onCancel: (() -> Void)?) {
        let completion = { () -> Void in
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            onChange?()
        }
        if let newLink = link.text, !newLink.isEmpty {
            addRecipeLink(link: newLink, active: active, completion: completion)
        } else {
            onCancel?()
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        link.addDoneButtonOnKeyboard()
    }
}
