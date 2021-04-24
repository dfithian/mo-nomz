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
    var onChange: (() -> Void)?
    
    @IBAction func didTapSearch(_ sender: Any) {
        let url: URL?
        if let linkText = link.text, let escaped = linkText.addingPercentEncoding(withAllowedCharacters: .urlHostAllowed) {
            link.text = ""
            url = URL(string: "https://google.com/search?q=\(escaped)")
        } else {
            url = URL(string: "https://google.com")!
        }
        if let u = url {
            present(SFSafariViewController(url: u), animated: true, completion: nil)
        }
    }

    @IBAction func didTapSave(_ sender: Any) {
        let completion = { () -> Void in
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.onChange?()
        }
        if let newLink = link.text, !newLink.isEmpty {
            addRecipeLink(link: link.text!, completion: completion)
        } else {
            didTapCancel(sender)
        }
    }
    
    @IBAction func didTapCancel(_ sender: Any) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        link.becomeFirstResponder()
        link.addDoneButtonOnKeyboard()
    }
}
