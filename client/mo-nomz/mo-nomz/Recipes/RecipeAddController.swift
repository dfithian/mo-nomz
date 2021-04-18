//
//  RecipeAddController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit

class RecipeAddController: UIViewController, UITextViewDelegate {
    @IBOutlet weak var link: UITextField!
    @IBOutlet weak var name: UITextField!
    @IBOutlet weak var content: UITextView!
    @IBOutlet weak var toolbar: UIToolbar!
    var onChange: (() -> Void)?
    var placeholderAttribution: NSAttributedString?
    var attribution: NSAttributedString?

    @IBAction func didTapSave(_ sender: Any) {
        let completion = { () -> Void in
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.onChange?()
        }
        if !(link.text?.isEmpty ?? true) {
            addRecipeLink(link: link.text!, completion: completion)
        } else if !content.text.isEmpty {
            let newName = (name.text?.isEmpty ?? true) ? "Untitled" : name.text!
            addRecipeBody(name: newName, content: content.text, completion: completion)
        }
    }
    
    @IBAction func didTapCancel(_ sender: Any) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    func textViewDidBeginEditing(_ textView: UITextView) {
        if textView.attributedText == placeholderAttribution {
            content.attributedText = attribution
            content.text = ""
        }
    }
    
    func textViewDidEndEditing(_ textView: UITextView) {
        if content.text == "" {
            content.attributedText = placeholderAttribution
            content.text = "Ingredients"
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        link.becomeFirstResponder()
        attribution = content.attributedText
        content.attributedText = name.attributedPlaceholder
        content.text = "Ingredients"
        placeholderAttribution = content.attributedText
        link.addDoneButtonOnKeyboard()
        name.addDoneButtonOnKeyboard()
        content.addDoneButtonOnKeyboard()
    }
}
