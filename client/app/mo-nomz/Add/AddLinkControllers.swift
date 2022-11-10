//
//  AddLinkController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import UIKit
import WebKit
import SafariServices

class AddLinkController: AddDetailController {
    @IBOutlet weak var link: UITextField!
    @IBOutlet weak var helper: UIButton!

    var url: URL? = nil
    
    override func addType() -> AddType {
        return .link
    }
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        if let text = link.text?.nonEmpty(), let u = URL(string: text) {
            url = u.toRecipeUrl() ?? u
            performSegue(withIdentifier: "review", sender: nil)
        } else {
            alertUnsuccessful("Please provide a link to a recipe.")
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? SelectLinkDataController, segue.identifier == "review" {
            vc.url = url
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        link.addDoneButtonOnKeyboard()
        helper.menu = switcherMenu()
    }
}

class SelectLinkDataController: SimpleController {
    @IBOutlet weak var header: UITextView!
    @IBOutlet weak var web: WKWebView!
    @IBOutlet weak var skip: UIBarButtonItem!
    
    var url: URL? = nil
    var scrapeType: ScrapeType = .ingredient
    var ingredients: String? = nil
    var steps: String? = nil
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        let str = UIPasteboard.general.string
        UIPasteboard.general.string = nil
        if (str?.nonEmpty()?.isEmpty ?? true) {
            switch scrapeType {
            case .ingredient:
                alertUnsuccessful("Copy ingredients to clipboard")
                break
            case .step:
                alertUnsuccessful("Copy steps to clipboard or select Skip")
            }
            return
        }
        switch (scrapeType) {
        case .ingredient:
            ingredients = str
            scrapeType = .step
            header.text = "Copy steps to clipboard."
            skip.isEnabled = true
            break
        case .step:
            steps = str
            performSegue(withIdentifier: "pushManualRecipe", sender: nil)
        }
    }
    
    @IBAction func didTapSkip(_ sender: Any?) {
        steps = nil
        performSegue(withIdentifier: "pushManualRecipe", sender: nil)
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddManualController, segue.identifier == "pushManualRecipe" {
            vc.change = .link(url?.absoluteString, web.title, ingredients ?? "", steps)
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        web.load(URLRequest(url: url!))
        UIPasteboard.general.string = nil
        header.text = "Copy ingredients to clipboard."
    }
}
