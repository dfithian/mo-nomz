//
//  RecipeDetailListController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 7/11/21.
//

import MobileCoreServices
import UIKit

enum Subentity: Codable {
    case ingredient(ReadableIngredientWithId)
    case step(StepWithId)
}

struct SubentityDragInfo {
    let indexPath: IndexPath
}

class RecipeDetailListController: UITableViewController, UITextViewDelegate, UITableViewDragDelegate, UITableViewDropDelegate {
    var recipe: ReadableRecipeWithId? = nil
    var steps: [StepWithId] = []
    var ingredients: [ReadableIngredientWithId] = []
    var onChange: (() -> Void)? = nil
    var beforeHeight: CGFloat? = nil
    var mergeItems: (ReadableIngredientWithId, ReadableIngredientWithId)? = nil
    var editItem: ReadableIngredientWithId? = nil
    
    let ADD_INGREDIENT_TAG = 0
    let ADD_STEP_TAG = 1

    let INGREDIENT_LIST_HEADING = 0
    let MERGE_TIP = 1
    let INGREDIENT_LIST = 2
    let ADD_INGREDIENT = 3
    let STEP_LIST_HEADING = 4
    let REORDER_STEP_TIP = 5
    let STEP_LIST = 6
    let ADD_STEP = 7
    let NOTES_HEADING = 8
    let NOTES = 9
    
    @objc func didTapAdd(_ sender: Any?) {
        guard let b = sender as? UIButton else { return }
        if b.tag == ADD_INGREDIENT_TAG {
            performSegue(withIdentifier: "addItem", sender: nil)
        } else {
            newStep()
        }
    }
    
    @objc func didEndEditingStep(_ sender: Any?) {
        guard let t = sender as? UITextField else { return }
        guard let r = recipe else { return }
        let step = steps[t.tag]
        let cell = tableView.cellForRow(at: IndexPath(row: t.tag, section: STEP_LIST)) as! StepItem
        cell.write.isEnabled = false
        cell.write.alpha = 0
        cell.read.alpha = 1
        cell.read.text = cell.write.text
        let new = StepWithId(id: step.id, step: Step(step: cell.write.text!, order: step.step.order))
        updateRecipeStep(recipeId: r.id, step: new)
        onChange?()
    }
    
    private func newStep() {
        guard let r = recipe else { return }
        let newSteps = addRecipeSteps(recipeId: r.id, rawSteps: [""])
        steps.append(contentsOf: newSteps)
        tableView.insertRows(at: [IndexPath(row: steps.count - 1, section: STEP_LIST)], with: .automatic)
    }
    
    func textViewDidEndEditing(_ textView: UITextView) {
        guard let r = recipe else { return }
        updateRecipe(id: r.id, recipe: ReadableRecipe(name: r.recipe.name, link: r.recipe.link, active: r.recipe.active, rating: r.recipe.rating, notes: textView.text ?? r.recipe.notes, ingredients: r.recipe.ingredients, steps: r.recipe.steps))
        onChange?()
    }

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 10
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case NOTES_HEADING: return 1
        case NOTES: return 1
        case MERGE_TIP: return !User.dismissedIngredientMergeTip() ? 1 : 0
        case INGREDIENT_LIST_HEADING: return 1
        case INGREDIENT_LIST: return ingredients.count
        case ADD_INGREDIENT: return 1
        case STEP_LIST_HEADING: return 1
        case REORDER_STEP_TIP: return !User.dismissedStepReorderTip() ? 1 : 0
        case STEP_LIST: return steps.count
        case ADD_STEP: return 1
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
        case INGREDIENT_LIST_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SimpleSectionHeader
            cell.label.text = "Ingredients"
            return cell
        case INGREDIENT_LIST:
            let cell = tableView.dequeueReusableCell(withIdentifier: "listItem") as! ListItem
            let item = ingredients[indexPath.row]
            cell.name.text = item.ingredient.render()
            return cell
        case ADD_INGREDIENT:
            let cell = tableView.dequeueReusableCell(withIdentifier: "addItem") as! AddItem
            cell.add.tag = ADD_INGREDIENT_TAG
            cell.add.addTarget(self, action: #selector(didTapAdd), for: .touchUpInside)
            return cell
        case STEP_LIST_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SimpleSectionHeader
            cell.label.text = "Steps"
            return cell
        case REORDER_STEP_TIP:
            return tableView.dequeueReusableCell(withIdentifier: "reorderTip")!
        case STEP_LIST:
            let cell = tableView.dequeueReusableCell(withIdentifier: "stepItem") as! StepItem
            cell.num.text = String(indexPath.row + 1)
            cell.read.text = steps[indexPath.row].step.step
            cell.write.text = steps[indexPath.row].step.step
            cell.write.tag = indexPath.row
            cell.write.addDoneButtonOnKeyboard()
            cell.write.addTarget(self, action: #selector(didEndEditingStep), for: .editingDidEnd)
            return cell
        case ADD_STEP:
            let cell = tableView.dequeueReusableCell(withIdentifier: "addItem") as! AddItem
            cell.add.tag = ADD_STEP_TAG
            cell.add.addTarget(self, action: #selector(didTapAdd), for: .touchUpInside)
            return cell
        default:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SimpleSectionHeader
            return cell
        }
    }
    
    override func tableView(_ tableView: UITableView, trailingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        guard let r = recipe else { return nil }
        let delete: Subentity
        switch indexPath.section {
        case INGREDIENT_LIST:
            delete = .ingredient(ingredients[indexPath.row])
            break
        case STEP_LIST:
            delete = .step(steps[indexPath.row])
            break
        default:
            return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { [weak self] (action, view, completionHandler) in
            switch delete {
            case .ingredient(let ingredient):
                self?.updateRecipeIngredients(id: r.id, active: r.recipe.active, deletes: [ingredient.id], adds: [])
                break
            case .step(let step):
                self?.deleteRecipeStep(id: step.id)
                break
            }
            self?.onChange?()
            completionHandler(true)
        }
        action.backgroundColor = .systemRed
        return UISwipeActionsConfiguration(actions: [action])
    }
    
    override func tableView(_ tableView: UITableView, leadingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        guard let r = recipe else { return nil }
        let delete: Subentity
        switch indexPath.section {
        case INGREDIENT_LIST:
            delete = .ingredient(ingredients[indexPath.row])
            break
        case STEP_LIST:
            delete = .step(steps[indexPath.row])
            break
        default:
            return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { [weak self] (action, view, completionHandler) in
            switch delete {
            case .ingredient(let ingredient):
                self?.updateRecipeIngredients(id: r.id, active: r.recipe.active, deletes: [ingredient.id], adds: [])
                break
            case .step(let step):
                self?.deleteRecipeStep(id: step.id)
                break
            }
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
        case INGREDIENT_LIST:
            editItem = ingredients[indexPath.row]
            performSegue(withIdentifier: "editItem", sender: nil)
            break
        case ADD_INGREDIENT:
            performSegue(withIdentifier: "addItem", sender: nil)
            break
        case REORDER_STEP_TIP:
            let handler = { (action: UIAlertAction) -> Void in
                User.setDidDismissStepReorderTip()
                DispatchQueue.main.async {
                    self.tableView.reloadData()
                }
            }
            promptForConfirmation(title: "Dismiss this tip", message: "Drag steps to reorder", handler: handler)
            break
        case STEP_LIST:
            let cell = tableView.cellForRow(at: indexPath) as! StepItem
            cell.write.isEnabled = true
            cell.write.becomeFirstResponder()
            cell.write.alpha = 1
            cell.read.alpha = 0
            break
        case ADD_STEP:
            performSegue(withIdentifier: "addStep", sender: nil)
            break
        default:
            break
        }
    }
    
    func tableView(_ tableView: UITableView, itemsForBeginning session: UIDragSession, at indexPath: IndexPath) -> [UIDragItem] {
        let item: Subentity
        switch indexPath.section {
        case INGREDIENT_LIST:
            item = .ingredient(ingredients[indexPath.row])
            break
        case STEP_LIST:
            item = .step(steps[indexPath.row])
            break
        default:
            return []
        }
        do {
            session.localContext = SubentityDragInfo(indexPath: indexPath)
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
        case INGREDIENT_LIST:
            break
        case STEP_LIST:
            break
        default: return cancel
        }
        if tableView.hasActiveDrag {
            if indexPath.section == INGREDIENT_LIST {
                return UITableViewDropProposal(operation: .move, intent: .insertIntoDestinationIndexPath)
            }
            if indexPath.section == STEP_LIST {
                return UITableViewDropProposal(operation: .move, intent: .insertAtDestinationIndexPath)
            }
        }
        return cancel
    }

    func tableView(_ tableView: UITableView, performDropWith coordinator: UITableViewDropCoordinator) {
        guard let indexPath = coordinator.destinationIndexPath else { return }
        guard let info = coordinator.session.localDragSession?.localContext as? SubentityDragInfo else { return }
        let existing: Subentity
        let newOrder: Int
        switch indexPath.section {
        case INGREDIENT_LIST:
            existing = .ingredient(ingredients[indexPath.row])
            newOrder = 0
            break
        case STEP_LIST:
            if info.indexPath.row < indexPath.row {
                let step = steps[indexPath.row]
                existing = .step(step)
                newOrder = step.step.order + 1
            } else if indexPath.row == 0 {
                let step = steps[indexPath.row]
                existing = .step(step)
                newOrder = step.step.order
            } else {
                let step = steps[indexPath.row - 1]
                existing = .step(step)
                newOrder = step.step.order + 1
            }
            break
        default:
            return
        }
        coordinator.session.loadObjects(ofClass: NSString.self, completion: { items in
            guard let strings = items as? [String] else { return }
            for string in strings {
                do {
                    let new = try JSONDecoder().decode(Subentity.self, from: string.data(using: .utf8)!)
                    let run = { () -> Void in
                        switch (existing, new) {
                        case (.ingredient(let x), .ingredient(let y)):
                            self.mergeItems = (x, y)
                            self.performSegue(withIdentifier: "mergeItems", sender: nil)
                            break
                        case (_, .step(let y)):
                            guard let r = self.recipe else { break }
                            let step = StepWithId(id: y.id, step: Step(step: y.step.step, order: newOrder))
                            self.updateRecipeStep(recipeId: r.id, step: step)
                            self.onChange?()
                            break
                        default:
                            break
                        }
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
        tableView.rowHeight = UITableView.automaticDimension
        tableView.estimatedRowHeight = 600
    }
}
