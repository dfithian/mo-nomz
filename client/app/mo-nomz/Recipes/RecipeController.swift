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

    var recipes: [ReadableRecipeWithId]? = nil
    var filterVc: RecipeFilterController? = nil
    var recipeVc: RecipeListController? = nil

    func loadData() {
        recipes = Database.selectRecipes()
        filterVc?.tags = Database.getTopTags()
        filterVc?.collectionView.reloadData()
        recipeVc?.allRecipes = recipes ?? []
        recipeVc?.onFilter()
        recipeVc?.tableView.reloadData()
    }

    func updateTags(active: Bool, tags: Set<String>) {
        recipeVc?.active = active
        recipeVc?.tags = tags
        recipeVc?.onFilter()
        recipeVc?.tableView.reloadData()
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddController, segue.identifier == "addGroceries" {
            vc.onChange = loadData
        }
        if let vc = segue.destination as? RecipeFilterController, segue.identifier == "embedTags" {
            filterVc = vc
            vc.delegate = self
            vc.tags = Database.getTopTags()
        }
        if let vc = segue.destination as? RecipeListController, segue.identifier == "embedRecipes" {
            recipeVc = vc
            loadData()
            vc.onChange = loadData
            search.delegate = vc
        }
        if let vc = segue.destination as? BannerController, segue.identifier == "embedBanner" {
            vc.height = banner.constraints.filter({ $0.identifier == "height" }).first
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        add.frame = CGRect(x: add.frame.minX, y: add.frame.minY, width: add.frame.width, height: toolbar.frame.height)
        add.alignTextUnderImage()
        search.searchTextField.addDoneButtonOnKeyboard()
        search.searchTextField.font = UIFont.systemFont(ofSize: 14)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadData()
    }
}
