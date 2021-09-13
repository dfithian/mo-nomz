//
//  RecipeDetailListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 7/11/21.
//

import MobileCoreServices
import UIKit

class RecipeDetailListController: UITableViewController, UITextViewDelegate, UITableViewDragDelegate, UITableViewDropDelegate {
    var recipe: ReadableRecipeWithId? = nil
    var ingredients: [ReadableIngredientWithId] = []
    var onChange: (() -> Void)? = nil
    var beforeHeight: CGFloat? = nil
    var mergeItems: (ReadableIngredientWithId, ReadableIngredientWithId)? = nil
    var editItem: ReadableIngredientWithId? = nil
    
    let NOTES_HEADING = 0
    let NOTES = 1
    let MERGE_TIP = 2
    let LIST_HEADING = 3
    let LIST = 4
    let ADD = 5
    
    @objc func didTapAdd(_ sender: Any?) {
        performSegue(withIdentifier: "addItem", sender: nil)
    }
    
    func textViewDidEndEditing(_ textView: UITextView) {
        guard let r = recipe else { return }
        updateRecipe(id: r.id, recipe: ReadableRecipe(name: r.recipe.name, link: r.recipe.link, active: r.recipe.active, rating: r.recipe.rating, notes: textView.text ?? r.recipe.notes, ingredients: r.recipe.ingredients))
        onChange?()
    }

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 6
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case NOTES_HEADING: return 1
        case NOTES: return 1
        case MERGE_TIP: return !User.dismissedIngredientMergeTip() ? 1 : 0
        case LIST_HEADING: return 1
        case LIST: return ingredients.count
        case ADD: return 1
        default: return 0
        }
    }

    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case NOTES_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SimpleSectionHeader
            cell.label.text = "Notes"
            return cell
        case NOTES:
            let cell = tableView.dequeueReusableCell(withIdentifier: "noteItem") as! NoteItem
            cell.blob.text = recipe?.recipe.notes ?? ""
            cell.blob.addDoneButtonOnKeyboard()
            cell.blob.layer.cornerRadius = 10
            cell.blob.delegate = self
            return cell
        case MERGE_TIP:
            return tableView.dequeueReusableCell(withIdentifier: "mergeTip")!
        case LIST_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SimpleSectionHeader
            cell.label.text = "Ingredients"
            return cell
        case LIST:
            let cell = tableView.dequeueReusableCell(withIdentifier: "listItem") as! IngredientListItem
            let item = ingredients[indexPath.row]
            cell.name.text = item.ingredient.render()
            return cell
        case ADD:
            let cell = tableView.dequeueReusableCell(withIdentifier: "addItem") as! AddItem
            cell.add.addTarget(self, action: #selector(didTapAdd), for: .touchUpInside)
            return cell
        default:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SimpleSectionHeader
            return cell
        }
    }
    
    override func tableView(_ tableView: UITableView, trailingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        guard let r = recipe else { return nil }
        let delete: UUID
        switch indexPath.section {
        case LIST:
            delete = ingredients[indexPath.row].id
            break
        default:
            return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { [weak self] (action, view, completionHandler) in
            self?.updateRecipeIngredients(id: r.id, active: r.recipe.active, deletes: [delete], adds: [])
            self?.onChange?()
            completionHandler(true)
        }
        action.backgroundColor = .systemRed
        return UISwipeActionsConfiguration(actions: [action])
    }
    
    override func tableView(_ tableView: UITableView, leadingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        guard let r = recipe else { return nil }
        let delete: UUID
        switch indexPath.section {
        case LIST:
            delete = ingredients[indexPath.row].id
            break
        default:
            return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { [weak self] (action, view, completionHandler) in
            self?.updateRecipeIngredients(id: r.id, active: r.recipe.active, deletes: [delete], adds: [])
            self?.onChange?()
            completionHandler(true)
        }
        action.backgroundColor = .systemRed
        return UISwipeActionsConfiguration(actions: [action])
    }
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch indexPath.section {
        case MERGE_TIP:
            let handler = { (action: UIAlertAction) -> Void in
                User.setDidDismissIngredientMergeTip()
                DispatchQueue.main.async {
                    self.tableView.reloadData()
                }
            }
            promptForConfirmation(title: "Dismiss this tip", message: "Drag items to merge", handler: handler)
            break
        case LIST:
            editItem = ingredients[indexPath.row]
            performSegue(withIdentifier: "editItem", sender: nil)
            break
        case ADD:
            performSegue(withIdentifier: "addItem", sender: nil)
            break
        default:
            break
        }
    }
    
    func tableView(_ tableView: UITableView, itemsForBeginning session: UIDragSession, at indexPath: IndexPath) -> [UIDragItem] {
        let item: ReadableIngredientWithId
        switch indexPath.section {
        case LIST:
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
        case LIST:
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
        let existing: ReadableIngredientWithId
        switch indexPath.section {
        case LIST:
            existing = ingredients[indexPath.row]
            break
        default:
            return
        }
        coordinator.session.loadObjects(ofClass: NSString.self, completion: { items in
            guard let strings = items as? [String] else { return }
            for string in strings {
                do {
                    let new = try JSONDecoder().decode(ReadableIngredientWithId.self, from: string.data(using: .utf8)!)
                    let run = { () -> Void in
                        self.mergeItems = (existing, new)
                        self.performSegue(withIdentifier: "mergeItems", sender: nil)
                    }
                    let runAndIgnore = { () -> Void in
                        User.setDidDismissIngredientMergeWarning()
                        run()
                    }
                    if !User.dismissedIngredientMergeWarning() {
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
