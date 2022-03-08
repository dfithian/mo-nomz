//
//  RecipeListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import MobileCoreServices
import UIKit

class RecipeListController: UITableViewController, UISearchBarDelegate {
    var active: Bool = false
    var tags: Set<String>? = nil
    var search: String? = nil
    var allRecipes: [ReadableRecipeWithId] = []
    var recipes: [ReadableRecipeWithId] = []
    var onChange: (() -> Void)?
    
    let EMPTY = 0
    let RECIPES = 1
    
    private func hasData() -> Bool {
        return allRecipes.count > 0
    }

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 2
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case EMPTY: return hasData() ? 0 : 1
        case RECIPES: return recipes.count
        default: return 0
        }
    }
    
    @objc func didTapActive(_ sender: Any?) {
        let b = sender as! UIButton
        let r = recipes[b.tag]
        Database.updateRecipe(id: r.id, recipe: ReadableRecipe(name: r.recipe.name, link: r.recipe.link, active: !r.recipe.active, rating: r.recipe.rating, notes: r.recipe.notes, ingredients: r.recipe.ingredients, steps: r.recipe.steps, tags: r.recipe.tags))
        onChange?()
    }
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch indexPath.section {
        case RECIPES:
            performSegue(withIdentifier: "showRecipe", sender: recipes[indexPath.row])
            break
        default:
            break
        }
    }
    
    func deleteRow(_ id: UUID) {
        Database.deleteRecipe(id: id)
        onChange?()
    }
    
    private func swipe(_ indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        let id: UUID
        switch indexPath.section {
        case RECIPES:
            id = recipes[indexPath.row].id
            break
        default:
            return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { (action, view, completionHandler) in
            self.deleteRow(id)
            completionHandler(true)
        }
        action.backgroundColor = .systemRed
        return UISwipeActionsConfiguration(actions: [action])
    }
    
    override func tableView(_ tableView: UITableView, trailingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        return swipe(indexPath)
    }
    
    override func tableView(_ tableView: UITableView, leadingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        return swipe(indexPath)
    }
    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case EMPTY:
            let cell = tableView.dequeueReusableCell(withIdentifier: "emptyItem")!
            return cell
        case RECIPES:
            let recipe = recipes[indexPath.row].recipe
            let cell: TwoLabelOneButton
            if recipe.active {
                cell = tableView.dequeueReusableCell(withIdentifier: "recipeListItem") as! TwoLabelOneButton
            } else {
                cell = tableView.dequeueReusableCell(withIdentifier: "savedListItem") as! TwoLabelOneButton
            }
            cell.oneLabel.text = recipe.name
            cell.button.tag = indexPath.row
            cell.button.addTarget(self, action: #selector(didTapActive), for: .touchUpInside)
            cell.twoLabel.text = String(recipe.rating)
            return cell
        default: return UITableViewCell()
        }
    }
    
    func onFilter() {
        recipes = allRecipes
        if active {
            recipes = recipes.filter({ $0.recipe.active })
        }
        for tag_ in tags ?? [] {
            recipes = recipes.filter({ $0.recipe.tags.contains(where: { $0 == tag_ }) })
        }
        if let searchText = search {
            let words = searchText.components(separatedBy: .whitespacesAndNewlines).filter({ !$0.isEmpty }).map({ $0.lowercased() })
            let recipeIds = search(items: recipes, tokens: words)
            recipes = recipeIds.compactMap({ (id) in recipes.first(where: { (recipe) in id == recipe.id }) })
        }
    }
    
    func searchBar(_ searchBar: UISearchBar, textDidChange searchText: String) {
        search = searchText.nonEmpty()
        onFilter()
        tableView.reloadData()
    }
    
    func searchBarSearchButtonClicked(_ searchBar: UISearchBar) {
        searchBar.searchTextField.endEditing(true)
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? RecipeDetailController, segue.identifier == "showRecipe", let r = sender as? ReadableRecipeWithId {
            vc.onChange = onChange
            vc.recipe = r
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()

        // This is here because we don't support drag and drop
        tableView.rowHeight = UITableView.automaticDimension
        tableView.estimatedRowHeight = 600
    }
}
