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

class RecipeDetailListController: UITableViewController, UITextViewDelegate, UITableViewDragDelegate, UITableViewDropDelegate {
    var recipe: RecipeWithId? = nil
    var ingredients: [IngredientWithId] = []
    var onChange: (() -> Void)? = nil
    var beforeHeight: CGFloat? = nil
    var mergeItems: (IngredientWithId, IngredientWithId)? = nil
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
    
    func tableView(_ tableView: UITableView, itemsForBeginning session: UIDragSession, at indexPath: IndexPath) -> [UIDragItem] {
        let item: IngredientWithId
        switch indexPath.section {
        case 3:
            item = ingredients[indexPath.row]
            break
        default:
            return []
        }
        do {
            let data = try JSONEncoder().encode(item)
            return [UIDragItem(itemProvider: NSItemProvider(item: data as NSData, typeIdentifier: kUTTypePlainText as String))]
        } catch {
            print("Failed to initiate drag and drop \(error)")
        }
        return []
    }

    func tableView(_ tableView: UITableView, dropSessionDidUpdate session: UIDropSession, withDestinationIndexPath destinationIndexPath: IndexPath?) -> UITableViewDropProposal {
        let cancel = UITableViewDropProposal(operation: .cancel)
        guard let indexPath = destinationIndexPath else { return cancel }
        guard session.items.count == 1 else { return cancel }
        switch indexPath.section {
        case 3:
            if indexPath.row < ingredients.count {
                tableView.scrollToRow(at: indexPath, at: .none, animated: true)
            } else {
                tableView.scrollToRow(at: IndexPath(row: ingredients.count - 1, section: indexPath.section), at: .none, animated: true)
            }
            break
        default: return cancel
        }
        if tableView.hasActiveDrag {
            return UITableViewDropProposal(operation: .move, intent: .insertIntoDestinationIndexPath)
        }
        return cancel
    }

    func tableView(_ tableView: UITableView, performDropWith coordinator: UITableViewDropCoordinator) {
        guard let indexPath = coordinator.destinationIndexPath else { return }
        let existing: IngredientWithId
        switch indexPath.section {
        case 3:
            existing = ingredients[indexPath.row]
            break
        default:
            return
        }
        coordinator.session.loadObjects(ofClass: NSString.self, completion: { items in
            guard let strings = items as? [String] else { return }
            for string in strings {
                do {
                    let new = try JSONDecoder().decode(IngredientWithId.self, from: string.data(using: .utf8)!)
                    let prefs = Persistence.loadPreferencess()
                    let run = { () -> Void in
                        self.mergeItems = (existing, new)
                        self.performSegue(withIdentifier: "mergeItems", sender: nil)
                    }
                    let runAndIgnore = { () -> Void in
                        Persistence.setPreferences(Preferences(dismissedMergeWarning: prefs.dismissedMergeWarning, dismissedIngredientMergeWarning: true))
                        run()
                    }
                    if !prefs.dismissedIngredientMergeWarning {
                        self.promptForConfirmationThree(
                            title: "Warning",
                            message: "Merging items may result in unexpected grocery list behavior. You may want to deactivate this recipe first.",
                            option1Button: "OK",
                            option1Handler: { _ in run() },
                            option2Button: "Ignore future warnings",
                            option2Handler: { _ in runAndIgnore() }
                        )
                    } else {
                        run()
                    }
                    break
                } catch {
                    print("Failed completing drag and drop \(error)")
                }
            }
        })
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? IngredientMergeController, segue.identifier == "mergeItems" {
            vc.recipe = recipe
            vc.existing = mergeItems!.0
            vc.new = mergeItems!.1
            vc.onChange = onChange
        }
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
    
    override func viewDidLoad() {
        super.viewDidLoad()
        tableView.dragDelegate = self
        tableView.dropDelegate = self
        tableView.dragInteractionEnabled = true
    }
}
