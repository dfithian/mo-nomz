//
//  RecipeAddController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit
import SafariServices

class RecipeAddController: UIViewController {
    @IBOutlet weak var label: UILabel!
    @IBOutlet weak var link: UITextField!
    @IBOutlet weak var inactive: UISwitch!
    @IBOutlet weak var indicator: UILabel!
    var existingLinks: [String] = []
    var active: Bool = true
    
    @IBAction func didTapSwitch(_ sender: Any) {
        if let s = sender as? UISwitch {
            updateActive(s.isOn)
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
    
    private func updateActive(_ val: Bool) {
        if val {
            active = true
            indicator.text = "Active"
        } else {
            active = false
            indicator.text = "Saved for later"
        }
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        link.addDoneButtonOnKeyboard()
        updateActive(true)
        inactive.isOn = true
    }
}
