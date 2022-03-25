//
//  AddManualController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/31/22.
//

import UIKit

enum ManualChange {
    case add
    case photoReview(String, String?)
}

class AddManualController: AddDetailController {
    @IBOutlet weak var back: UIButton!

    var change: ManualChange? = nil
    var manualVc: AddManualTableController? = nil
    
    @IBAction func didTapBack(_ sender: Any?) {
        DispatchQueue.main.async {
            self.navigationVc?.popViewController(animated: true)
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
        if let i = manualVc?.ingredients?.text?.nonEmpty() {
            if manualVc?.isRecipe ?? false {
                if let n = manualVc?.name?.text?.nonEmpty() {
                    addBlob(content: i, name: n, link: manualVc?.link?.text?.nonEmpty(), rawSteps: manualVc?.steps?.text?.nonEmpty()?.components(separatedBy: "\n").compactMap({ $0.nonEmpty() }) ?? [], active: manualVc?.isActive ?? true, completion: recipeCompletion)
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
    
    override func addType() -> AddType {
        switch change {
        case .photoReview(_, _): return .photo
        default: return .manual
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddManualTableController, segue.identifier == "embedManual" {
            manualVc = vc
            switch change {
            case .photoReview(let ingredients, let steps):
                vc.ingredients = UITextView()
                vc.ingredients?.text = ingredients
                vc.steps = UITextView()
                vc.steps?.text = steps
                if steps?.nonEmpty() != nil {
                    vc.isRecipe = true
                }
                break
            default: break
            }
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        switch change {
        case .photoReview(_, _):
            back.alpha = 1
            break
        default: break
        }
    }
}

class AddManualTableController: UITableViewController, UITextViewDelegate {
    var isRecipe: Bool = false
    var isActive: Bool = true
    var name: UITextField? = nil
    var link: UITextField? = nil
    var ingredients: UITextView? = nil
    var steps: UITextView? = nil
    
    let HEADER = 0
    let IS_RECIPE__IS_ACTIVE = 1
    let NAME = 2
    let LINK = 3
    let INGREDIENT_HEADING = 4
    let INGREDIENTS = 5
    let STEP_HEADING = 6
    let STEPS = 7
    
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
        tableView.reloadSections(sections as IndexSet, with: .automatic)
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
    
    func textViewDidEndEditing(_ textView: UITextView) {
        tableView.reloadData()
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        return 8
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case HEADER: return 1
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
        case HEADER:
            return tableView.dequeueReusableCell(withIdentifier: "header")!
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
            cell.text_.text = name?.text
            cell.text_.addDoneButtonOnKeyboard()
            name = cell.text_
            return cell
        case LINK:
            let cell = tableView.dequeueReusableCell(withIdentifier: "textFieldItem") as! OneTextField
            cell.text_.placeholder = "Recipe link (optional)"
            cell.text_.text = link?.text
            cell.text_.addDoneButtonOnKeyboard()
            link = cell.text_
            return cell
        case INGREDIENT_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! OneLabel
            if isRecipe {
                cell.label.text = "Ingredients"
            } else {
                cell.label.text = "Groceries"
            }
            return cell
        case INGREDIENTS:
            let cell = tableView.dequeueReusableCell(withIdentifier: "textItem") as! OneText
            cell.text_.text = ingredients?.text
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.layer.cornerRadius = 10
            cell.text_.delegate = self
            ingredients = cell.text_
            return cell
        case STEP_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! OneLabel
            cell.label.text = "Steps"
            return cell
        case STEPS:
            let cell = tableView.dequeueReusableCell(withIdentifier: "textItem") as! OneText
            cell.text_.text = steps?.text
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.layer.cornerRadius = 10
            cell.text_.delegate = self
            steps = cell.text_
            return cell
        default:
            return UITableViewCell()
        }
    }
    
    override func tableView(_ tableView: UITableView, heightForRowAt indexPath: IndexPath) -> CGFloat {
        switch indexPath.section {
        case INGREDIENTS: return max(ingredients?.contentSize.height ?? 0, 100)
        case STEPS: return max(steps?.contentSize.height ?? 0, 100)
        default: return UITableView.automaticDimension
        }
    }
}
