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
    @IBOutlet weak var add: UIButton!

    var recipeVc: RecipeListController? = nil
    
    @objc func triggerAdd(_ sender: Any) {
        performSegue(withIdentifier: "addGroceries", sender: sender)
    }
    
    @IBAction func clear(_ sender: Any?) {
        let items : [RecipeWithId] = (recipeVc?.active ?? []) + (recipeVc?.saved ?? [])
        if !items.isEmpty {
            let handler = { [weak self] (action: UIAlertAction) -> Void in self?.clearGroceryItems(completion: self?.loadData) }
            promptForConfirmation(title: "Clear", message: "Are you sure you want to clear?", handler: handler)
        }
    }

    @objc func loadData() {
        let completion = { [weak self] (resp: ListRecipeResponse) -> Void in
            let recipes = resp.recipes.map({
                RecipeWithId(recipe: $0.value, id: $0.key)
            })
            self?.recipeVc?.active = recipes.filter({ $0.recipe.active }).sorted(by: { $0.id < $1.id })
            self?.recipeVc?.saved = recipes.filter({ !$0.recipe.active }).sorted(by: { $0.id < $1.id })
            DispatchQueue.main.async {
                self?.recipeVc?.tableView.reloadData()
            }
        }
        loadRecipes(completion: completion)
    }

    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? GroceryAddController, segue.identifier == "addGroceries" {
            vc.onChange = { () -> Void in
                self.loadData()
            }
        }
        if let vc = segue.destination as? RecipeListController, segue.identifier == "embedRecipes" {
            recipeVc = vc
            loadData()
            vc.onChange = { () -> Void in
                self.loadData()
            }
        }
        if let vc = segue.destination as? BannerController, segue.identifier == "embedBanner" {
            vc.height = banner.constraints.filter({ $0.identifier == "height" }).first
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        clear.frame = CGRect(x: clear.frame.minX, y: clear.frame.minY, width: clear.frame.width, height: toolbar.frame.height)
        clear.alignTextUnderImage()
        add.frame = CGRect(x: add.frame.minX, y: add.frame.minY, width: add.frame.width, height: toolbar.frame.height)
        add.alignTextUnderImage()
        NotificationCenter.default.addObserver(self, selector: #selector(loadData), name: UIApplication.willEnterForegroundNotification, object: nil)
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadData()
    }
}
