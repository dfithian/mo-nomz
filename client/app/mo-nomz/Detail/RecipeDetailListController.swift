//
//  RecipeDetailListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 7/11/21.
//

import MobileCoreServices
import UIKit

struct IngredientWithId: Codable {
    let id: Int
    let ingredient: ReadableIngredient
}

class RecipeDetailListController: UITableViewController, UITextViewDelegate {
    var recipe: RecipeWithId? = nil
    var ingredients: [IngredientWithId] = []
    var onChange: (() -> Void)? = nil
    var beforeHeight: CGFloat? = nil
    var editItem: IngredientWithId? = nil
    
    @objc func didTapAdd(_ sender: Any?) {
        performSegue(withIdentifier: "addItem", sender: nil)
    }
    
    func textViewDidEndEditing(_ textView: UITextView) {
        guard let r = recipe else { return }
        updateRecipe(id: r.id, active: r.recipe.active, rating: r.recipe.rating, notes: textView.text ?? r.recipe.notes, completion: onChange)
    }

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 5
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case 0: return 1
        case 1: return 1
        case 2: return 1
        case 4: return 1
        default: return ingredients.count
        }
    }

    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case 0:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SimpleSectionHeader
            cell.label.text = "Notes"
            return cell
        case 1:
            let cell = tableView.dequeueReusableCell(withIdentifier: "noteItem") as! NoteItem
            cell.blob.text = recipe?.recipe.notes ?? ""
            cell.blob.addDoneButtonOnKeyboard()
            cell.blob.layer.cornerRadius = 10
            cell.blob.delegate = self
            return cell
        case 2:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SimpleSectionHeader
            cell.label.text = "Ingredients"
            return cell
        case 4:
            let cell = tableView.dequeueReusableCell(withIdentifier: "addItem") as! AddItem
            cell.add.addTarget(self, action: #selector(didTapAdd), for: .touchUpInside)
            return cell
        default:
            let cell = tableView.dequeueReusableCell(withIdentifier: "listItem") as! IngredientListItem
            let item = ingredients[indexPath.row]
            cell.name.text = item.ingredient.render()
            return cell
        }
    }
    
    override func tableView(_ tableView: UITableView, trailingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        guard let r = recipe else { return nil }
        let delete: Int
        switch indexPath.section {
        case 3:
            delete = ingredients[indexPath.row].id
            break
        default:
            return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { [weak self] (action, view, completionHandler) in
            self?.updateRecipeIngredients(id: r.id, deletes: [delete], adds: [], completion: self?.onChange)
            completionHandler(true)
        }
        action.backgroundColor = .systemRed
        return UISwipeActionsConfiguration(actions: [action])
    }
    
    override func tableView(_ tableView: UITableView, leadingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        guard let r = recipe else { return nil }
        let delete: Int
        switch indexPath.section {
        case 3:
            delete = ingredients[indexPath.row].id
            break
        default:
            return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { [weak self] (action, view, completionHandler) in
            self?.updateRecipeIngredients(id: r.id, deletes: [delete], adds: [], completion: self?.onChange)
            completionHandler(true)
        }
        action.backgroundColor = .systemRed
        return UISwipeActionsConfiguration(actions: [action])
    }
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch indexPath.section {
        case 3:
            editItem = ingredients[indexPath.row]
            performSegue(withIdentifier: "editItem", sender: nil)
            break
        case 4:
            performSegue(withIdentifier: "addItem", sender: nil)
            break
        default:
            break
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? IngredientEditController, segue.identifier == "editItem" {
            vc.recipe = recipe
            vc.existing = editItem
            vc.onChange = onChange
        }
        if let vc = segue.destination as? IngredientAddController, segue.identifier == "addItem" {
            vc.recipe = recipe
            vc.order = recipe?.recipe.ingredients.map({ $0.value.order }).max().map({ $0 + 1 })
            vc.onChange = onChange
        }
    }
}
