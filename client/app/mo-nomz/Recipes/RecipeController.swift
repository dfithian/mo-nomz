//
//  RecipeController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import UIKit

class RecipeController: UIViewController {
    @IBOutlet weak var banner: UIView!
    @IBOutlet weak var toolbar: Toolbar!
    @IBOutlet weak var clear: UIButton!
    @IBOutlet weak var export: UIButton!
    @IBOutlet weak var help: UIButton!
    @IBOutlet weak var add: UIButton!
    @IBOutlet weak var search: UISearchBar!

    var recipeVc: RecipeListController? = nil
    
    @objc func triggerAdd(_ sender: Any) {
        performSegue(withIdentifier: "addGroceries", sender: sender)
    }
    
    @IBAction func export(_ sender: Any?) {
        let items: [ReadableGroceryItemWithId] = Database.selectGroceries().filter({ $0.item.active })
        if !items.isEmpty {
            let exportText: String = items.map({ $0.item.render() }).joined(separator: "\n")
            let vc = UIActivityViewController(activityItems: [exportText], applicationActivities: nil)
            present(vc, animated: true, completion: nil)
        }
    }
    
    @IBAction func help(_ sender: Any?) {
        needHelp()
    }
    
    @IBAction func clear(_ sender: Any?) {
        let items : [ReadableRecipeWithId] = (recipeVc?.active ?? []) + (recipeVc?.saved ?? [])
        if !items.isEmpty {
            let handler = { [weak self] (action: UIAlertAction) -> Void in
                Database.clearAll()
                self?.loadData()
            }
            promptForConfirmation(title: "Clear", message: "Are you sure you want to clear?", handler: handler)
        }
    }

    @objc func loadData() {
        let recipes = Database.selectRecipes()
        recipeVc?.allActive = recipes.filter({ $0.recipe.active })
        recipeVc?.allSaved = recipes.filter({ !$0.recipe.active })
        recipeVc?.onSearch()
        DispatchQueue.main.async {
            self.recipeVc?.tableView.reloadData()
        }
    }

    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddController, segue.identifier == "addGroceries" {
            vc.onChange = loadData
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
        clear.frame = CGRect(x: clear.frame.minX, y: clear.frame.minY, width: clear.frame.width, height: toolbar.frame.height)
        clear.alignTextUnderImage()
        export.frame = CGRect(x: export.frame.minX, y: export.frame.minY, width: export.frame.width, height: toolbar.frame.height)
        export.alignTextUnderImage()
        help.frame = CGRect(x: help.frame.minX, y: help.frame.minY, width: help.frame.width, height: toolbar.frame.height)
        help.alignTextUnderImage()
        add.frame = CGRect(x: add.frame.minX, y: add.frame.minY, width: add.frame.width, height: toolbar.frame.height)
        add.alignTextUnderImage()
        search.searchTextField.addDoneButtonOnKeyboard()
        search.searchTextField.font = UIFont.systemFont(ofSize: 14)
        NotificationCenter.default.addObserver(self, selector: #selector(loadData), name: UIApplication.willEnterForegroundNotification, object: nil)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadData()
    }
}
