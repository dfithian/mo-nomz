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
    @IBOutlet weak var add: UIButton!
    @IBOutlet weak var search: UISearchBar!

    var filterVc: RecipeFilterController? = nil
    var recipeVc: RecipeListController? = nil

    func reloadData() {
        recipeVc?.reloadData()
        filterVc?.reloadData()
    }

    func updateSelectedTag(active: Bool, tag: String?) {
        recipeVc?.active = active
        recipeVc?.tag = tag
        recipeVc?.onFilter()
        recipeVc?.tableView.reloadData()
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddController, segue.identifier == "addGroceries" {
            vc.onChange = reloadData
        }
        if let vc = segue.destination as? RecipeFilterController, segue.identifier == "embedTags" {
            filterVc = vc
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
