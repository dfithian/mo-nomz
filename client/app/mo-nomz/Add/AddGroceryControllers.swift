//
//  AddGroceryControllers.swift
//  mo-nomz
//
//  Created by Dan Fithian on 6/17/23.
//

import UIKit

class AddGroceryController: AddDetailController, UITextViewDelegate {
    @IBOutlet weak var text: UITextView!
    
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
        text.addDoneButtonOnKeyboard()
        text.becomeFirstResponder()
    }
}
