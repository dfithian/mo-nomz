//
//  AddManualController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/31/22.
//

import UIKit

enum ManualChange {
    case add
    case link(String?, String?, String, String?)
    case photo(String, String?)
}

class AddManualController: AddDetailController {
    @IBOutlet weak var header: UITextView!
    @IBOutlet weak var helper: UIButton!
    @IBOutlet weak var noHelperConstraint: NSLayoutConstraint!
    @IBOutlet weak var helperConstraint: NSLayoutConstraint!

    var change: ManualChange = .add
    var manualVc: AddManualTableController? = nil
    
    override func addType() -> AddType {
        switch (change) {
        case .add: return .manual
        case .link(_, _, _, _): return .link
        case .photo(_, _): return .photo
        }
    }
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        let recipeCompletion = { (recipe: ReadableRecipeWithId) in
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.navigationVc?.onChange?()
            self.loadRecipe(recipe)
        }
        let groceryCompletion = {
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.navigationVc?.onChange?()
            self.loadGroceries()
        }
        if let i = manualVc?.ingredients?.nonEmpty() {
            if manualVc?.isRecipe ?? false {
                if let n = manualVc?.name?.nonEmpty() {
                    addBlob(content: i, name: n, link: manualVc?.link?.nonEmpty(), rawSteps: manualVc?.steps?.nonEmpty()?.components(separatedBy: "\n").compactMap({ $0.nonEmpty() }) ?? [], active: manualVc?.isActive ?? true, completion: recipeCompletion)
                } else {
                    alertUnsuccessful("Please provide a name.")
                }
            } else {
                addBlob(content: i, completion: { _ in groceryCompletion() })
            }
        } else {
            alertUnsuccessful("Please provide ingredients.")
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddManualTableController, segue.identifier == "embedManual" {
            manualVc = vc
            switch change {
            case .link(let link, let name, let ingredients, let steps):
                vc.isRecipe = true
                vc.link = link
                vc.name = name
                vc.ingredients = ingredients
                vc.steps = steps
            case .photo(let ingredients, let steps):
                vc.isRecipe = true
                vc.ingredients = ingredients
                vc.steps = steps
                break
            default: break
            }
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        switch (change) {
        case .link(_, _, _, _):
            header.text = "Review selections."
            helper.removeFromSuperview()
            noHelperConstraint.isActive = true
            helperConstraint.isActive = false
            break
        case .photo(_, _):
            header.text = "Review selections."
            helper.removeFromSuperview()
            noHelperConstraint.isActive = true
            helperConstraint.isActive = false
            break
        case .add:
            header.text = "Add ingredients or a recipe to your list."
            helper.menu = switcherMenu()
            break
        }
    }
}

class AddManualTableController: UITableViewController, UITextFieldDelegate, UITextViewDelegate {
    var isRecipe: Bool = false
    var isActive: Bool = true
    var name: String? = nil
    var link: String? = nil
    var ingredients: String? = nil
    var steps: String? = nil
    
    let IS_RECIPE__IS_ACTIVE = 0
    let NAME = 1
    let LINK = 2
    let INGREDIENT_HEADING = 3
    let INGREDIENTS = 4
    let STEP_HEADING = 5
    let STEPS = 6
    
    @objc func didTapIsRecipe(_ sender: Any?) {
        let b = sender as! UIButton
        if isRecipe {
            isRecipe = false
            b.setImage(UIImage(systemName: "square"), for: .normal)
        } else {
            isRecipe = true
            b.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
        }
        let range = NSMakeRange(NAME, tableView.numberOfSections - NAME)
        let sections = NSIndexSet(indexesIn: range)
        DispatchQueue.main.async {
            self.tableView.reloadSections(sections as IndexSet, with: .automatic)
        }
    }
    
    @objc func didTapIsActive(_ sender: Any?) {
        let b = sender as! UIButton
        if isActive {
            isActive = false
            b.setImage(UIImage(systemName: "square"), for: .normal)
        } else {
            isActive = true
            b.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
        }
    }
    
    func textFieldDidEndEditing(_ textField: UITextField) {
        switch (textField.tag) {
        case NAME:
            name = textField.text
            break
        case LINK:
            link = textField.text
            break
        default: break
        }
    }
    
    func textViewDidChange(_ textView: UITextView) {
        switch (textView.tag) {
        case INGREDIENTS:
            ingredients = textView.text
            break
        case STEPS:
            steps = textView.text
            break
        default: break
        }
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        return 7
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case IS_RECIPE__IS_ACTIVE: return 1
        case NAME: return isRecipe ? 1 : 0
        case LINK: return isRecipe ? 1 : 0
        case INGREDIENT_HEADING: return 1
        case INGREDIENTS: return 1
        case STEP_HEADING: return isRecipe ? 1 : 0
        case STEPS: return isRecipe ? 1 : 0
        default: return 0
        }
    }
    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case IS_RECIPE__IS_ACTIVE:
            let cell = tableView.dequeueReusableCell(withIdentifier: "checkboxItem") as! TwoButton
            cell.one.setImage(UIImage(systemName: isActive ? "checkmark.square" : "square"), for: .normal)
            cell.one.addTarget(self, action: #selector(didTapIsActive), for: .touchUpInside)
            cell.two.setImage(UIImage(systemName: isRecipe ? "checkmark.square" : "square"), for: .normal)
            cell.two.addTarget(self, action: #selector(didTapIsRecipe), for: .touchUpInside)
            return cell
        case NAME:
            let cell = tableView.dequeueReusableCell(withIdentifier: "textFieldItem") as! OneTextField
            cell.text_.placeholder = "Recipe name"
            cell.text_.text = name
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.tag = NAME
            cell.text_.delegate = self
            return cell
        case LINK:
            let cell = tableView.dequeueReusableCell(withIdentifier: "textFieldItem") as! OneTextField
            cell.text_.placeholder = "Recipe link (optional)"
            cell.text_.text = link
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.tag = LINK
            cell.text_.delegate = self
            return cell
        case INGREDIENT_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! OneLabel
            cell.label.text = isRecipe ? "Ingredients" : "Groceries"
            return cell
        case INGREDIENTS:
            let cell = tableView.dequeueReusableCell(withIdentifier: "textItem") as! OneText
            cell.text_.text = ingredients
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.layer.cornerRadius = 10
            cell.text_.tag = INGREDIENTS
            cell.text_.delegate = self
            return cell
        case STEP_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! OneLabel
            cell.label.text = "Steps"
            return cell
        case STEPS:
            let cell = tableView.dequeueReusableCell(withIdentifier: "textItem") as! OneText
            cell.text_.text = steps
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.layer.cornerRadius = 10
            cell.text_.tag = STEPS
            cell.text_.delegate = self
            return cell
        default:
            return UITableViewCell()
        }
    }
}
