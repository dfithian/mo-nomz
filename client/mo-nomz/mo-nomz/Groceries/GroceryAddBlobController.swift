//
//  GroceryAddBlobController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/23/21.
//

import UIKit

class GroceryAddBlobController: UIViewController {
    var onChange: (() -> Void)? = nil

    @IBOutlet weak var blob: UITextView!
    
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @IBAction func didTapSave(_ sender: Any?) {
        if let content = blob.text, !content.isEmpty {
            let completion = {
                DispatchQueue.main.async {
                    self.dismiss(animated: true, completion: nil)
                }
                self.onChange?()
            }
            addGroceryBlob(content: content, completion: completion)
        } else {
            didTapCancel(sender)
        }
    }
    
    override func viewDidLoad() {
        blob.becomeFirstResponder()
        blob.addDoneButtonOnKeyboard()
    }
}
