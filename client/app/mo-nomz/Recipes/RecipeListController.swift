//
//  RecipeListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/13/21.
//

import MobileCoreServices
import UIKit

class RecipeListController: UITableViewController {
    var active: [ReadableRecipeWithId] = []
    var saved: [ReadableRecipeWithId] = []
    var onChange: (() -> Void)?
    var collapsed: [Bool] = [false, true]
    
    private func hasData() -> Bool {
        return (active.count + saved.count) > 0
    }

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 5
    }
    
    private func collapsedSection(_ section: Int) -> Int? {
        switch section {
        case 2: return 0
        case 4: return 1
        default: return nil
        }
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        if let c = collapsedSection(section) {
            if collapsed[c] { return 0 }
        }
        switch section {
        case 0: return hasData() ? 0 : 1
        case 1: return hasData() ? 1 : 0
        case 2: return active.count
        case 3: return hasData() ? 1 : 0
        case 4: return saved.count
        default: return 1
        }
    }
    
    @objc func didTapActive(_ sender: Any?) {
        let b = sender as! UIButton
        let r = active[b.tag]
        updateRecipe(id: r.id, recipe: ReadableRecipe(name: r.recipe.name, link: r.recipe.link, active: false, rating: r.recipe.rating, notes: r.recipe.notes, ingredients: [:]), completion: onChange)
    }
    
    @objc func didTapSavedForLater(_ sender: Any?) {
        let b = sender as! UIButton
        let r = saved[b.tag]
        updateRecipe(id: r.id, recipe: ReadableRecipe(name: r.recipe.name, link: r.recipe.link, active: true, rating: r.recipe.rating, notes: r.recipe.notes, ingredients: [:]), completion: onChange)
    }
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch indexPath.section {
        case 1:
            collapsed[0] = !collapsed[0]
            DispatchQueue.main.async {
                self.tableView.reloadData()
            }
            break
        case 2:
            performSegue(withIdentifier: "showRecipe", sender: indexPath)
            break
        case 3:
            collapsed[1] = !collapsed[1]
            DispatchQueue.main.async {
                self.tableView.reloadData()
            }
            break
        case 4:
            performSegue(withIdentifier: "showRecipe", sender: indexPath)
            break
        default:
            break
        }
    }
    
    func deleteRow(_ id: UUID) {
        deleteRecipe(id: id, completion: onChange)
    }
    
    override func tableView(_ tableView: UITableView, trailingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        let id: UUID
        switch indexPath.section {
        case 2:
            id = active[indexPath.row].id
            break
        case 4:
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
    
    override func tableView(_ tableView: UITableView, leadingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        let id: UUID
        switch indexPath.section {
        case 2:
            id = active[indexPath.row].id
            break
        case 4:
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
    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case 0:
            let cell = tableView.dequeueReusableCell(withIdentifier: "emptyItem")!
            return cell
        case 1:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SectionHeader
            let image = collapsed[0] ? UIImage(systemName: "chevron.forward.circle.fill") : UIImage(systemName: "chevron.down.circle.fill")
            cell.indicator.setImage(image, for: .normal)
            cell.label.text = "Active (\(active.count))"
            return cell
        case 2:
            let cell = tableView.dequeueReusableCell(withIdentifier: "recipeListItem") as! RecipeListItem
            let recipe = active[indexPath.row].recipe
            cell.name.text = recipe.name
            cell.select.tag = indexPath.row
            cell.select.addTarget(self, action: #selector(didTapActive), for: .touchUpInside)
            cell.rating.text = String(recipe.rating)
            return cell
        case 3:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SectionHeader
            let image = collapsed[1] ? UIImage(systemName: "chevron.forward.circle.fill") : UIImage(systemName: "chevron.down.circle.fill")
            cell.indicator.setImage(image, for: .normal)
            cell.label.text = "Saved for later (\(saved.count))"
            return cell
        case 4:
            let cell = tableView.dequeueReusableCell(withIdentifier: "savedListItem") as! RecipeListItem
            let recipe = saved[indexPath.row].recipe
            cell.name.text = recipe.name
            cell.select.tag = indexPath.row
            cell.select.addTarget(self, action: #selector(didTapSavedForLater), for: .touchUpInside)
            cell.rating.text = String(recipe.rating)
            return cell
        default:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SectionHeader
            return cell
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? RecipeDetailController, segue.identifier == "showRecipe", let indexPath = sender as? IndexPath {
            vc.onChange = onChange
            switch indexPath.section {
            case 2:
                vc.recipe = active[indexPath.row]
                break
            case 4:
                vc.recipe = saved[indexPath.row]
                break
            default: break
            }
        }
    }
}
