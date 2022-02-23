//
//  RecipeListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import MobileCoreServices
import UIKit

enum RecipeMode {
    case normal
    case search(String)
}

class RecipeListController: UITableViewController, UISearchBarDelegate {
    var mode: RecipeMode = .normal
    var allActive: [ReadableRecipeWithId] = []
    var allSaved: [ReadableRecipeWithId] = []
    var active: [ReadableRecipeWithId] = []
    var saved: [ReadableRecipeWithId] = []
    var onChange: (() -> Void)?
    var collapsed: [Bool] = [false, true]
    
    let EMPTY = 0
    let ACTIVE_HEADING = 1
    let ACTIVE = 2
    let SAVED_HEADING = 3
    let SAVED = 4
    
    private func hasData() -> Bool {
        return (allActive.count + allSaved.count) > 0
    }
    
    private func collapsed(_ i: Int) -> Bool {
        switch mode {
        case .normal: return collapsed[i]
        case .search(_): return false
        }
    }
    
    private func setCollapsed(_ i: Int) {
        switch mode {
        case .normal:
            collapsed[i] = !collapsed[i]
            break
        case .search(_):
            break
        }
    }

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 5
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case EMPTY: return hasData() ? 0 : 1
        case ACTIVE_HEADING: return hasData() ? 1 : 0
        case ACTIVE: return collapsed(0) ? 0 : active.count
        case SAVED_HEADING: return hasData() ? 1 : 0
        case SAVED: return collapsed(1) ? 0 : saved.count
        default: return 0
        }
    }
    
    @objc func didTapActive(_ sender: Any?) {
        let b = sender as! UIButton
        let r = active[b.tag]
        Database.updateRecipe(id: r.id, recipe: ReadableRecipe(name: r.recipe.name, link: r.recipe.link, active: false, rating: r.recipe.rating, notes: r.recipe.notes, ingredients: r.recipe.ingredients, steps: r.recipe.steps))
        onChange?()
    }
    
    @objc func didTapSavedForLater(_ sender: Any?) {
        let b = sender as! UIButton
        let r = saved[b.tag]
        Database.updateRecipe(id: r.id, recipe: ReadableRecipe(name: r.recipe.name, link: r.recipe.link, active: true, rating: r.recipe.rating, notes: r.recipe.notes, ingredients: r.recipe.ingredients, steps: r.recipe.steps))
        onChange?()
        tableView.reloadData()
    }
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch indexPath.section {
        case ACTIVE_HEADING:
            setCollapsed(0)
            DispatchQueue.main.async {
                self.tableView.reloadData()
            }
            break
        case ACTIVE:
            performSegue(withIdentifier: "showRecipe", sender: indexPath)
            break
        case SAVED_HEADING:
            setCollapsed(1)
            DispatchQueue.main.async {
                self.tableView.reloadData()
            }
            break
        case SAVED:
            performSegue(withIdentifier: "showRecipe", sender: indexPath)
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
        let action = UIContextualAction(style: .destructive, title: "Delete") { [weak self] (action, view, completionHandler) in
            self?.deleteRow(id)
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
        case ACTIVE_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! LabelButton
            let image = collapsed(0) ? UIImage(systemName: "chevron.forward.circle.fill") : UIImage(systemName: "chevron.down.circle.fill")
            cell.button.setImage(image, for: .normal)
            cell.label.text = "Active (\(active.count))"
            return cell
        case ACTIVE:
            let cell = tableView.dequeueReusableCell(withIdentifier: "recipeListItem") as! TwoLabelOneButton
            let recipe = active[indexPath.row].recipe
            cell.oneLabel.text = recipe.name
            cell.button.tag = indexPath.row
            cell.button.addTarget(self, action: #selector(didTapActive), for: .touchUpInside)
            cell.twoLabel.text = String(recipe.rating)
            return cell
        case SAVED_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! LabelButton
            let image = collapsed(1) ? UIImage(systemName: "chevron.forward.circle.fill") : UIImage(systemName: "chevron.down.circle.fill")
            cell.button.setImage(image, for: .normal)
            cell.label.text = "Saved for later (\(saved.count))"
            return cell
        case SAVED:
            let cell = tableView.dequeueReusableCell(withIdentifier: "savedListItem") as! TwoLabelOneButton
            let recipe = saved[indexPath.row].recipe
            cell.oneLabel.text = recipe.name
            cell.button.tag = indexPath.row
            cell.button.addTarget(self, action: #selector(didTapSavedForLater), for: .touchUpInside)
            cell.twoLabel.text = String(recipe.rating)
            return cell
        default: return UITableViewCell()
        }
    }
    
    func onSearch() {
        switch mode {
        case .normal:
            active = allActive
            saved = allSaved
            break
        case .search(let searchText):
            let words = searchText.components(separatedBy: .whitespacesAndNewlines).filter({ !$0.isEmpty }).map({ $0.lowercased() })
            let activeIds = search(items: allActive, tokens: words)
            let savedIds = search(items: allSaved, tokens: words)
            active = activeIds.compactMap({ (id) in allActive.first(where: { (recipe) in id == recipe.id }) })
            saved = savedIds.compactMap({ (id) in allSaved.first(where: { (recipe) in id == recipe.id }) })
        }
    }
    
    func searchBar(_ searchBar: UISearchBar, textDidChange searchText: String) {
        mode = searchText.isEmpty ? .normal : .search(searchText)
        onSearch()
        tableView.reloadData()
    }
    
    func searchBarSearchButtonClicked(_ searchBar: UISearchBar) {
        searchBar.searchTextField.endEditing(true)
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? RecipeDetailController, segue.identifier == "showRecipe", let indexPath = sender as? IndexPath {
            vc.onChange = onChange
            switch indexPath.section {
            case ACTIVE:
                vc.recipe = active[indexPath.row]
                break
            case SAVED:
                vc.recipe = saved[indexPath.row]
                break
            default: break
            }
        }
        if let vc = segue.destination as? RecipeDetailController, segue.identifier == "showRecipe", let r = sender as? ReadableRecipeWithId {
            vc.onChange = onChange
            vc.recipe = r
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        tableView.rowHeight = UITableView.automaticDimension
        tableView.estimatedRowHeight = 600
    }
}
