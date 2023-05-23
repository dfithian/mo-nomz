//
//  RecipeController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import UIKit

class RecipeController: UIViewController, RecipeFilterDelegate {
    @IBOutlet weak var add: UIButton!
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
    
    private func setupAdd() {
        add.showsMenuAsPrimaryAction = true
        add.menu = UIMenu(options: .displayInline, children: [
            UIAction(title: "Add recipe link", image: UIImage(systemName: "link"), handler: { _ in
                self.performSegue(withIdentifier: "addLink", sender: nil)
            }),
            UIAction(title: "Add manually", image: UIImage(systemName: "pencil"), handler: { _ in
                self.performSegue(withIdentifier: "addManual", sender: nil)
            }),
            UIAction(title: "Add recipe photos", image: UIImage(systemName: "photo.on.rectangle"), handler: { _ in
                self.performSegue(withIdentifier: "addPhoto", sender: nil)
            })
        ])
    }
    
    private func setupHamburger() {
        hamburger.showsMenuAsPrimaryAction = true
        hamburger.menu = UIMenu(options: .displayInline, children: [
            UIAction(title: "Clear", image: UIImage(systemName: "arrow.3.trianglepath"), attributes: .destructive, handler: { _ in
                if !Database.selectGroceries().isEmpty {
                    let handler = { (action: UIAlertAction) -> Void in
                        Database.clearAll()
                        self.reloadData()
                    }
                    self.promptForConfirmation(title: "Clear", message: "This will delete all groceries and deactivate all recipes. Do you want to continue?", handler: handler)
                }
            }),
            UIAction(title: "Settings", image: UIImage(systemName: "gear"), handler: { _ in
                self.performSegue(withIdentifier: "showSettings", sender: nil)
            })
        ])
    }

    func updateSelectedTag(tag: String?) {
        recipeVc?.tag = tag
        recipeVc?.onFilter()
        DispatchQueue.main.async {
            self.recipeVc?.tableView.reloadData()
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddController, segue.identifier == "addLink" {
            vc.addType = .link
            vc.onChange = reloadData
        }
        if let vc = segue.destination as? AddController, segue.identifier == "addManual" {
            vc.addType = .manualRecipes
            vc.onChange = reloadData
        }
        if let vc = segue.destination as? AddController, segue.identifier == "addPhoto" {
            vc.addType = .photo
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
        if let vc = segue.destination as? GroceryController, segue.identifier == "showGroceries" {
            vc.onChange = reloadData
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        reloadData()
        setupAdd()
        setupHamburger()
        search.searchTextField.addDoneButtonOnKeyboard()
        search.searchTextField.font = UIFont.systemFont(ofSize: 14)
    }
}
