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
    
    var type: String {
        switch self {
        case .ingredient(_): return Subentity.ingredientType
        case .step(_): return Subentity.stepType
        }
    }
    
    static var ingredientType = "ingredient"
    static var stepType = "step"
}

struct SubentityDragInfo {
    let indexPath: IndexPath
}

class RecipeDetailListController: UITableViewController, UITextViewDelegate, UITableViewDragDelegate, UITableViewDropDelegate {
    var recipe: ReadableRecipeWithId? = nil
    var steps: [StepWithId] = []
    var ingredients: [ReadableIngredientWithId] = []
    var onChange: (() -> Void)? = nil
    
    let ADD_INGREDIENT_TAG = 0
    let ADD_STEP_TAG = 1

    let INGREDIENT_LIST = 0
    let ADD_INGREDIENT = 1
    let STEP_LIST = 2
    let ADD_STEP = 3
    let NOTES = 4
    
    @objc func didTapAdd(_ sender: Any?) {
        guard let b = sender as? UIButton else { return }
        if b.tag == ADD_INGREDIENT_TAG {
            performSegue(withIdentifier: "addItem", sender: IngredientChange.add(recipe?.recipe.ingredients.map({ $0.value.order }).max().map({ $0 + 1}) ?? 1))
        } else {
            newStep()
        }
    }
    
    private func newStep() {
        guard let r = recipe else { return }
        let newSteps = Database.addRecipeSteps(recipeId: r.id, rawSteps: [""])
        steps.append(contentsOf: newSteps)
        let indexPath = IndexPath(row: steps.count - 1, section: STEP_LIST)
        tableView.insertRows(at: [indexPath], with: .automatic)
        editStep(indexPath)
    }
    
    private func editStep(_ indexPath: IndexPath) {
        let cell = tableView.cellForRow(at: indexPath) as! OneText
        cell.text_.becomeFirstResponder()
    }
    
    func textViewDidEndEditing(_ textView: UITextView) {
        guard let r = recipe else { return }
        if textView.tag == Int.max {
            Database.updateRecipe(id: r.id, recipe: ReadableRecipe(name: r.recipe.name, link: r.recipe.link, active: r.recipe.active, rating: r.recipe.rating, notes: textView.text ?? r.recipe.notes, ingredients: r.recipe.ingredients, steps: r.recipe.steps, tags: r.recipe.tags))
        } else {
            let step = steps[textView.tag]
            let cell = tableView.cellForRow(at: IndexPath(row: textView.tag, section: STEP_LIST)) as! OneText
            if let newStep = cell.text_.text?.trimmingCharacters(in: .whitespacesAndNewlines).nonEmpty() {
                let new = StepWithId(id: step.id, step: Step(step: newStep, order: step.step.order))
                Database.updateRecipeStep(recipeId: r.id, step: new)
            } else {
                Database.deleteRecipeStep(id: step.id)
            }
        }
        onChange?()
    }

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 5
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case NOTES: return 1
        case INGREDIENT_LIST: return ingredients.count
        case ADD_INGREDIENT: return 1
        case STEP_LIST: return steps.count
        case ADD_STEP: return 1
        default: return 0
        }
    }
    
    override func tableView(_ tableView: UITableView, titleForHeaderInSection section: Int) -> String? {
        switch section {
        case NOTES: return "Notes"
        case INGREDIENT_LIST: return "Ingredients"
        case STEP_LIST: return "Steps"
        default: return nil
        }
    }

    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case NOTES:
            let cell = tableView.dequeueReusableCell(withIdentifier: "noteItem") as! OneText
            cell.text_.text = recipe?.recipe.notes ?? ""
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.layer.cornerRadius = 10
            cell.text_.delegate = self
            cell.text_.tag = Int.max
            return cell
        case INGREDIENT_LIST:
            let cell = tableView.dequeueReusableCell(withIdentifier: "listItem") as! OneLabel
            let item = ingredients[indexPath.row]
            cell.label.text = item.ingredient.render()
            return cell
        case ADD_INGREDIENT:
            let cell = tableView.dequeueReusableCell(withIdentifier: "addItem") as! OneButton
            cell.button.tag = ADD_INGREDIENT_TAG
            cell.button.addTarget(self, action: #selector(didTapAdd), for: .touchUpInside)
            return cell
        case STEP_LIST:
            let cell = tableView.dequeueReusableCell(withIdentifier: "stepItem") as! OneText
            cell.text_.text = steps[indexPath.row].step.step
            cell.text_.tag = indexPath.row
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.delegate = self
            return cell
        case ADD_STEP:
            let cell = tableView.dequeueReusableCell(withIdentifier: "addItem") as! OneButton
            cell.button.tag = ADD_STEP_TAG
            cell.button.addTarget(self, action: #selector(didTapAdd), for: .touchUpInside)
            return cell
        default: return UITableViewCell()
        }
    }
    
    private func swipe(_ indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        guard let r = recipe else { return nil }
        let delete: Subentity
        switch indexPath.section {
        case INGREDIENT_LIST:
            delete = .ingredient(ingredients[indexPath.row])
            break
        case STEP_LIST:
            delete = .step(steps[indexPath.row])
            break
        default: return nil
        }
        let action = UIContextualAction(style: .destructive, title: "Delete") { (action, view, completionHandler) in
            switch delete {
            case .ingredient(let ingredient):
                Database.updateRecipeIngredients(id: r.id, active: r.recipe.active, deletes: [ingredient.id], adds: [])
                break
            case .step(let step):
                Database.deleteRecipeStep(id: step.id)
                break
            }
            self.onChange?()
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
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch indexPath.section {
        case INGREDIENT_LIST:
            performSegue(withIdentifier: "editItem", sender: IngredientChange.edit(ingredients[indexPath.row]))
            break
        case ADD_INGREDIENT:
            performSegue(withIdentifier: "addItem", sender: IngredientChange.add(recipe?.recipe.ingredients.map({ $0.value.order }).max().map({ $0 + 1}) ?? 1))
            break
        case STEP_LIST:
            editStep(indexPath)
            break
        case ADD_STEP:
            newStep()
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
        case INGREDIENT_LIST: break
        case STEP_LIST: break
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
                            self.performSegue(withIdentifier: "mergeItems", sender: IngredientChange.merge(x, y))
                            break
                        case (_, .step(let y)):
                            guard let r = self.recipe else { break }
                            let step = StepWithId(id: y.id, step: Step(step: y.step.step, order: newOrder))
                            Database.updateRecipeStep(recipeId: r.id, step: step)
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
                    if !User.dismissedIngredientMergeWarning() && new.type == Subentity.ingredientType {
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
        if let vc = segue.destination as? IngredientChangeController, ["mergeItems", "editItem", "addItem"].contains(segue.identifier), let change = sender as? IngredientChange {
            vc.recipe = recipe
            vc.onChange = onChange
            vc.change = change
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        tableView.dragDelegate = self
        tableView.dropDelegate = self
        tableView.dragInteractionEnabled = true

        // This is here because we allow drag and drop, but only merge, so the cell sizes are not recalculated
        tableView.rowHeight = UITableView.automaticDimension
        tableView.estimatedRowHeight = 600
    }
}
