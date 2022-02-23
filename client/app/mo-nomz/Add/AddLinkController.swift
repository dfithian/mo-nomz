//
//  AddLinkController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit
import SafariServices

class AddLinkController: AddDetailController {
    @IBOutlet weak var link: UITextField!
    @IBOutlet weak var checkbox: UIButton!

    var active: Bool = true
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        let completion = {
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.navigationVc?.onChange?()
        }
        if let newLink = link.text?.nonEmpty() {
            addLink(link: newLink, active: active, completion: { _ in completion() })
        } else {
            alertUnsuccessful("Please provide a link.")
        }
    }
    
    @IBAction func didTapIsActive(_ sender: Any?) {
        if active {
            active = false
            checkbox.setImage(UIImage(systemName: "square"), for: .normal)
        } else {
            active = true
            checkbox.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
        }
    }
    
    override func addType() -> AddType {
        return .link
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        link.addDoneButtonOnKeyboard()
    }
}
