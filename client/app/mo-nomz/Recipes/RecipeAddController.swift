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
    
    @IBAction func didTapSwitch(_ sender: Any) {
        if let s = sender as? UISwitch {
            updateActive(!s.isOn)
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
        inactive.isOn = false
    }
}
