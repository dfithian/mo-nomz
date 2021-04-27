//
//  GroceryAddBlobController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/23/21.
//

import UIKit

class GroceryAddBlobController: UIViewController {
    @IBOutlet weak var blob: UITextView!
    
    func save(_ onChange: (() -> Void)?, onCancel: (() -> Void)?) {
        if let content = blob.text, !content.isEmpty {
            let completion = {
                DispatchQueue.main.async {
                    self.dismiss(animated: true, completion: nil)
                }
                onChange?()
            }
            addGroceryBlob(content: content, completion: completion)
        } else {
            onCancel?()
        }
    }
    
    override func viewDidLoad() {
        blob.becomeFirstResponder()
        blob.addDoneButtonOnKeyboard()
    }
}
