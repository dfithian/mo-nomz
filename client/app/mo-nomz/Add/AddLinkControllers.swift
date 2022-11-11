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
            performSegue(withIdentifier: "reviewIngredients", sender: nil)
        } else {
            alertUnsuccessful("Please provide a link to a recipe.")
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? SelectLinkIngredientsController, segue.identifier == "reviewIngredients" {
            vc.url = url
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        link.addDoneButtonOnKeyboard()
        helper.menu = switcherMenu()
    }
}

protocol SelectLinkDelegate {
    func selectionWasEmpty() -> Void
    func selectionWasSuccess(_ selection: String) -> Void
}

class SelectLinkController: SimpleController, WKNavigationDelegate {
    @IBOutlet weak var web: WKWebView!
    
    var delegate: SelectLinkDelegate? = nil
    
    var progress: UIActivityIndicatorView? = nil
    var url: URL? = nil
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        let str = UIPasteboard.general.string
        UIPasteboard.general.string = nil
        guard let scraped = str?.nonEmpty() else {
            delegate?.selectionWasEmpty()
            return
        }
        delegate?.selectionWasSuccess(scraped)
    }
    
    func webView(_ webView: WKWebView, didCommit navigation: WKNavigation!) {
        guard let p = progress else { return }
        stopLoading(p)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        progress = startLoading()
        web.navigationDelegate = self
        web.load(URLRequest(url: url!))
        UIPasteboard.general.string = nil
    }
}

class SelectLinkIngredientsController: SelectLinkController, SelectLinkDelegate {
    var scraped: String? = nil
    
    func selectionWasEmpty() {
        alertUnsuccessful("Copy ingredients to clipboard")
    }
    
    func selectionWasSuccess(_ selection: String) {
        scraped = selection
        performSegue(withIdentifier: "reviewSteps", sender: nil)
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? SelectLinkStepsController, segue.identifier == "reviewSteps" {
            vc.url = url
            vc.ingredients = scraped
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        delegate = self
    }
}

class SelectLinkStepsController: SelectLinkController, SelectLinkDelegate {
    @IBOutlet weak var skip: UIBarButtonItem!
    
    var ingredients: String? = nil
    var scraped: String? = nil
    
    func selectionWasEmpty() {
        alertUnsuccessful("Copy steps to clipboard or select Skip")
    }
    
    func selectionWasSuccess(_ selection: String) {
        scraped = selection
        performSegue(withIdentifier: "pushManualRecipe", sender: nil)
    }
    
    @IBAction func didTapSkip(_ sender: Any?) {
        performSegue(withIdentifier: "pushManualRecipe", sender: nil)
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddManualController, segue.identifier == "pushManualRecipe" {
            vc.change = .link(url?.absoluteString, web.title, ingredients ?? "", scraped)
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        delegate = self
    }
}
