//
//  RecipeController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import UIKit

class RecipeController: UIViewController, RecipeFilterDelegate {
    @IBOutlet weak var banner: UIView!
    @IBOutlet weak var toolbar: Toolbar!
    @IBOutlet weak var clear: UIButton!
    @IBOutlet weak var export: UIButton!
    @IBOutlet weak var options: UIButton!
    @IBOutlet weak var add: UIButton!
    @IBOutlet weak var search: UISearchBar!

    var filterVc: RecipeFilterController? = nil
    var recipeVc: RecipeListController? = nil
    
    @IBAction func didTapClear(_ sender: Any?) {
        if !Database.selectGroceries().isEmpty {
            let handler = { (action: UIAlertAction) -> Void in
                Database.clearAll()
                self.reloadData()
            }
            promptForConfirmation(title: "Clear", message: "Are you sure you want to clear?", handler: handler)
        }
    }
    
    @IBAction func didTapClearTags(_ sender: Any?) {
        filterVc?.clear()
    }

    func reloadData() {
        recipeVc?.reloadData()
        filterVc?.reloadData()
    }

    func updateSelectedTag(active: Bool, tag: String?) {
        recipeVc?.active = active
        recipeVc?.tag = tag
        recipeVc?.onFilter()
        DispatchQueue.main.async {
            self.recipeVc?.tableView.reloadData()
        }
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
        clear.frame = CGRect(x: clear.frame.minX, y: clear.frame.minY, width: clear.frame.width, height: toolbar.frame.height)
        clear.alignTextUnderImage()
        export.frame = CGRect(x: export.frame.minX, y: export.frame.minY, width: export.frame.width, height: toolbar.frame.height)
        export.alignTextUnderImage()
        options.frame = CGRect(x: options.frame.minX, y: options.frame.minY, width: options.frame.width, height: toolbar.frame.height)
        options.alignTextUnderImage()
        add.frame = CGRect(x: add.frame.minX, y: add.frame.minY, width: add.frame.width, height: toolbar.frame.height)
        add.alignTextUnderImage()
        search.searchTextField.addDoneButtonOnKeyboard()
        search.searchTextField.font = UIFont.systemFont(ofSize: 14)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        reloadData()
    }
}
