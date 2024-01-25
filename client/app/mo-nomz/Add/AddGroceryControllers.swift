//
//  AddGroceryControllers.swift
//  mo-nomz
//
//  Created by Dan Fithian on 6/17/23.
//

import UIKit

enum GroceryAdd {
    case manual
    case photo
}

class AddGroceryController: AddDetailController, UITextViewDelegate {
    @IBOutlet weak var header: UILabel!
    @IBOutlet weak var text: UITextView!
    
    var change: GroceryAdd = .manual
    var ingredients: String? = nil

    @IBAction func didTapSubmit(_ sender: Any?) {
        let groceryCompletion = {
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.navigationVc?.onChange?()
            self.loadGroceries()
        }
        if let i = ingredients?.nonEmpty() {
            addBlob(content: i, completion: { _ in groceryCompletion() })
        } else {
            alertUnsuccessful("Please provide ingredients.")
        }
    }
    
    func textViewDidChange(_ textView: UITextView) {
        ingredients = textView.text
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        switch (change) {
        case .manual:
            header.text = "Add groceries"
            break
        case .photo:
            header.text = "Review selections"
            break
        }
        text.text = ingredients ?? ""
        text.addDoneButtonOnKeyboard()
        text.becomeFirstResponder()
    }
}
