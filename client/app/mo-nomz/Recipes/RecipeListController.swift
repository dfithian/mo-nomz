//
//  RecipeListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import MobileCoreServices
import UIKit

class RecipeListController: UITableViewController, UISearchBarDelegate {
    var tag: String? = nil
    var search: String? = nil
    var allRecipes: [ReadableRecipeWithId] = []
    var active: [ReadableRecipeWithId] = []
    var saved: [ReadableRecipeWithId] = []
    var onChange: (() -> Void)?
    
    let ACTIVE = 0
    let SAVED = 1
    
    private func hasData() -> Bool {
        return allRecipes.count > 0
    }

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 2
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case ACTIVE: return active.count
        case SAVED: return saved.count
        default: return 0
        }
    }
    
    @objc func didTapSave(_ sender: Any?) {
        let b = sender as! UIButton
        let r = active[b.tag]
        Database.updateRecipe(id: r.id, recipe: ReadableRecipe(name: r.recipe.name, link: r.recipe.link, active: false, rating: r.recipe.rating, notes: r.recipe.notes, ingredients: r.recipe.ingredients, steps: r.recipe.steps, tags: r.recipe.tags))
        onChange?()
    }
    
    @objc func didTapActivate(_ sender: Any?) {
        let b = sender as! UIButton
        let r = saved[b.tag]
        Database.updateRecipe(id: r.id, recipe: ReadableRecipe(name: r.recipe.name, link: r.recipe.link, active: true, rating: r.recipe.rating, notes: r.recipe.notes, ingredients: r.recipe.ingredients, steps: r.recipe.steps, tags: r.recipe.tags))
        onChange?()
    }
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch indexPath.section {
        case ACTIVE:
            performSegue(withIdentifier: "showRecipe", sender: active[indexPath.row])
            break
        case SAVED:
            performSegue(withIdentifier: "showRecipe", sender: saved[indexPath.row])
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
        case ACTIVE:
            id = active[indexPath.row].id
            break
        case SAVED:
            id = saved[indexPath.row].id
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
        case ACTIVE:
            let recipe = active[indexPath.row].recipe
            let cell = tableView.dequeueReusableCell(withIdentifier: "recipeListItem") as! TwoLabelOneButton
            cell.oneLabel.text = recipe.name
            cell.button.tag = indexPath.row
            cell.button.addTarget(self, action: #selector(didTapSave), for: .touchUpInside)
            cell.twoLabel.text = String(recipe.rating)
            return cell
        case SAVED:
            let recipe = saved[indexPath.row].recipe
            let cell = tableView.dequeueReusableCell(withIdentifier: "savedListItem") as! TwoLabelOneButton
            cell.oneLabel.text = recipe.name
            cell.button.tag = indexPath.row
            cell.button.addTarget(self, action: #selector(didTapActivate), for: .touchUpInside)
            cell.twoLabel.text = String(recipe.rating)
            return cell
        default: return UITableViewCell()
        }
    }
    
    func onFilter() {
        var recipes = allRecipes
        if let tag_ = tag {
            recipes = recipes.filter({ $0.recipe.tags.contains(tag_) })
        }
        if let searchText = search {
            let words = searchText.components(separatedBy: .whitespacesAndNewlines).filter({ !$0.isEmpty }).map({ $0.lowercased() })
            let recipeIds = search(items: recipes, tokens: words)
            recipes = recipeIds.compactMap({ (id) in recipes.first(where: { (recipe) in id == recipe.id }) })
        }
        active = recipes.filter({ $0.recipe.active })
        saved = recipes.filter({ !$0.recipe.active })
    }
    
    func searchBar(_ searchBar: UISearchBar, textDidChange searchText: String) {
        search = searchText.nonEmpty()
        onFilter()
        let indexes = IndexSet(arrayLiteral: ACTIVE, SAVED)
        DispatchQueue.main.async {
            self.tableView.reloadSections(indexes, with: .automatic)
        }
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
    
    private func loadData() {
        allRecipes = Database.selectRecipes()
        onFilter()
    }
    
    func reloadData() {
        loadData()
        DispatchQueue.main.async {
            self.tableView.reloadData()
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        loadData()

        // This is here because we don't support drag and drop
        tableView.rowHeight = UITableView.automaticDimension
        tableView.estimatedRowHeight = 600
    }
}
