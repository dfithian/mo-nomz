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
    var onChange: (() -> Void)?
    var existingLinks: [String] = []
    
    @IBAction func didTapSearch(_ sender: Any) {
        var clearSearch: Bool = false
        var url: URL? = nil
        if let linkText = link.text {
            if let linkUrl = URL(string: linkText), ["http", "https"].contains(linkUrl.scheme) {
                url = linkUrl
            } else if let escaped = linkText.addingPercentEncoding(withAllowedCharacters: .urlHostAllowed) {
                clearSearch = true
                url = URL(string: "https://google.com/search?q=\(escaped)")
            } else {
                clearSearch = true
                url = URL(string: "https://google.com")
            }
        } else {
            clearSearch = true
            url = URL(string: "https://google.com")
        }
        if let u = url {
            if clearSearch {
                link.text = ""
            }
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
            addRecipeLink(link: newLink, completion: completion)
        } else {
            didTapCancel(sender)
        }
    }
    
    @IBAction func didTapCancel(_ sender: Any) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @objc func keyboardWillShow(notification: NSNotification) {
        keyboardWillShowInternal(view: label, notification: notification)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        link.addDoneButtonOnKeyboard()
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }
}
