//
//  GroceryAddBlobController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/23/21.
//

import UIKit

class GroceryAddBlobController: UIViewController {
    @IBOutlet weak var indicator: UILabel!
    @IBOutlet weak var inactive: UISwitch!
    @IBOutlet weak var name: UITextField!
    @IBOutlet weak var blob: UITextView!
    var active: Bool = true
    
    @IBAction func didTapSwitch(_ sender: Any) {
        if let s = sender as? UISwitch {
            updateActive(s.isOn)
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
            if let name = name.text, name != "" {
                addGroceryBlob(name: name, content: content, active: active, completion: completion)
            } else {
                addGroceryBlob(name: nil, content: content, active: true, completion: completion)
            }
        } else {
            onCancel?()
        }
    }
    
    private func updateActive(_ val: Bool) {
        if val {
            active = true
            indicator.text = "Active"
        } else {
            active = false
            indicator.text = "Saved for later"
        }
    }
    
    override func viewDidLoad() {
        name.addDoneButtonOnKeyboard()
        blob.addDoneButtonOnKeyboard()
        updateActive(true)
        inactive.isOn = true
    }
}
