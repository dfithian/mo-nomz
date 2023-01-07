//
//  RecipeController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import UIKit

class RecipeController: UIViewController, RecipeFilterDelegate {
    @IBOutlet weak var hamburger: UIButton!
    @IBOutlet weak var banner: UIView!
    @IBOutlet weak var search: UISearchBar!

    var filterVc: RecipeFilterController? = nil
    var recipeVc: RecipeListController? = nil
    
    @IBAction func didTapClearTags(_ sender: Any?) {
        filterVc?.clear()
    }

    func reloadData() {
        recipeVc?.reloadData()
        filterVc?.reloadData()
    }

    func updateSelectedTag(tag: String?) {
        recipeVc?.tag = tag
        recipeVc?.onFilter()
        DispatchQueue.main.async {
            self.recipeVc?.tableView.reloadData()
        }
    }
    
    private func setupHamburger() {
        hamburger.showsMenuAsPrimaryAction = true
        hamburger.menu = UIMenu(children: [
            UIAction(title: "Clear List", image: UIImage(systemName: "archivebox"), attributes: .destructive, handler: { _ in
                if !Database.selectGroceries().isEmpty {
                    let handler = { (action: UIAlertAction) -> Void in
                        Database.clearAll()
                        self.reloadData()
                    }
                    self.promptForConfirmation(title: "Clear", message: "This will delete all groceries and deactivate all recipes. Do you want to continue?", handler: handler)
                }
            })
        ])
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddController, segue.identifier == "addGroceries" {
            vc.onChange = reloadData
        }
        if let vc = segue.destination as? RecipeFilterController, segue.identifier == "embedTags" {
            filterVc = vc
            vc.onChange = reloadData
            vc.delegate = self
        }
        if let vc = segue.destination as? RecipeListController, segue.identifier == "embedRecipes" {
            recipeVc = vc
            vc.onChange = reloadData
            search.delegate = vc
        }
        if let vc = segue.destination as? BannerController, segue.identifier == "embedBanner" {
            vc.height = banner.constraints.filter({ $0.identifier == "height" }).first
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        reloadData()
        setupHamburger()
        search.searchTextField.addDoneButtonOnKeyboard()
        search.searchTextField.font = UIFont.systemFont(ofSize: 14)
    }
}
