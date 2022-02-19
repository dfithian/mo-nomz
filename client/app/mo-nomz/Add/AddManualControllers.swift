//
//  AddManualController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/31/22.
//

import UIKit

enum ManualChange {
    case add
    case photoReview
}

class AddManualController: AddDetailController {
    @IBOutlet weak var back: UIButton!

    var change: ManualChange? = nil
    var name: String? = nil
    var link: String? = nil
    var ingredients: String? = nil
    var steps: String? = nil
    var isRecipe: Bool = false
    var isActive: Bool = true
    var manualVc: AddManualTableController? = nil
    
    @IBAction func didTapBack(_ sender: Any?) {
        DispatchQueue.main.async {
            self.navigationVc?.popViewController(animated: true)
        }
    }
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        let completion = {
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.navigationVc?.onChange?()
        }
        if let i = ingredients {
            if isRecipe {
                if let n = name {
                    addBlob(content: i, name: n, link: link, rawSteps: steps?.nonEmpty()?.components(separatedBy: "\n").compactMap({ $0.nonEmpty() }) ?? [], active: isActive, completion: completion)
                } else {
                    alertUnsuccessful("Please provide a name.")
                }
            } else {
                addBlob(content: i, completion: completion)
            }
        } else {
            alertUnsuccessful("Please provide ingredients.")
        }
    }
    
    override func addType() -> AddType {
        return change == .photoReview ? .photo : .manual
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddManualTableController, segue.identifier == "embedManual" {
            manualVc = vc
            vc.parentVc = self
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        switch change {
        case .photoReview:
            back.alpha = 1
            break
        default: break
        }
    }
}

class AddManualTableController: UITableViewController, UITextViewDelegate, UITextFieldDelegate {
    var parentVc: AddManualController? = nil

    let INGREDIENTS_TAG = 0
    let STEPS_TAG = 1
    
    let NAME_TAG = 0
    let LINK_TAG = 1
    
    let INFO_HEADING = 0
    let IS_RECIPE__IS_ACTIVE = 1
    let NAME = 2
    let LINK = 3
    let INGREDIENT_HEADING = 4
    let INGREDIENTS = 5
    let STEP_HEADING = 6
    let STEPS = 7
    
    @objc func didTapIsRecipe(_ sender: Any?) {
        let b = sender as! UIButton
        if parentVc?.isRecipe ?? false {
            parentVc?.isRecipe = false
            b.setImage(UIImage(systemName: "square"), for: .normal)
        } else {
            parentVc?.isRecipe = true
            b.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
        }
        tableView.reloadData()
    }
    
    @objc func didTapIsActive(_ sender: Any?) {
        let b = sender as! UIButton
        if parentVc?.isActive ?? true {
            parentVc?.isActive = false
            b.setImage(UIImage(systemName: "square"), for: .normal)
        } else {
            parentVc?.isActive = true
            b.setImage(UIImage(systemName: "checkmark.square"), for: .normal)
        }
    }
    
    func textViewDidEndEditing(_ textView: UITextView) {
        switch textView.tag {
        case INGREDIENTS_TAG:
            parentVc?.ingredients = textView.text
            break
        case STEPS_TAG:
            parentVc?.steps = textView.text
            break
        default: break
        }
    }
    
    func textFieldDidEndEditing(_ textField: UITextField) {
        switch textField.tag {
        case NAME_TAG:
            parentVc?.name = textField.text
            break
        case LINK_TAG:
            parentVc?.link = textField.text
            break
        default: break
        }
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        return 8
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case INFO_HEADING: return 1
        case IS_RECIPE__IS_ACTIVE: return 1
        case NAME: return (parentVc?.isRecipe ?? false) ? 1 : 0
        case LINK: return (parentVc?.isRecipe ?? false) ? 1 : 0
        case INGREDIENT_HEADING: return 1
        case INGREDIENTS: return 1
        case STEP_HEADING: return (parentVc?.isRecipe ?? false) ? 1 : 0
        case STEPS: return (parentVc?.isRecipe ?? false) ? 1 : 0
        default: return 0
        }
    }
    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case INFO_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! OneLabel
            cell.label.text = "Info"
            return cell
        case IS_RECIPE__IS_ACTIVE:
            let cell = tableView.dequeueReusableCell(withIdentifier: "checkboxItem") as! TwoButton
            cell.one.setImage(UIImage(systemName: (parentVc?.isActive ?? true) ? "checkmark.square" : "square"), for: .normal)
            cell.one.addTarget(self, action: #selector(didTapIsActive), for: .touchUpInside)
            cell.two.setImage(UIImage(systemName: (parentVc?.isRecipe ?? false) ? "checkmark.square" : "square"), for: .normal)
            cell.two.addTarget(self, action: #selector(didTapIsRecipe), for: .touchUpInside)
            return cell
        case NAME:
            let cell = tableView.dequeueReusableCell(withIdentifier: "textFieldItem") as! OneTextField
            cell.text_.placeholder = "Recipe name"
            cell.text_.tag = NAME_TAG
            cell.text_.delegate = self
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.text = parentVc?.name
            return cell
        case LINK:
            let cell = tableView.dequeueReusableCell(withIdentifier: "textFieldItem") as! OneTextField
            cell.text_.placeholder = "Recipe link (optional)"
            cell.text_.tag = LINK_TAG
            cell.text_.delegate = self
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.text = parentVc?.link
            return cell
        case INGREDIENT_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! OneLabel
            if parentVc?.isRecipe ?? false {
                cell.label.text = "Ingredients"
            } else {
                cell.label.text = "Groceries"
            }
            return cell
        case INGREDIENTS:
            let cell = tableView.dequeueReusableCell(withIdentifier: "textItem") as! OneText
            cell.text_.tag = INGREDIENTS_TAG
            cell.text_.delegate = self
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.layer.cornerRadius = 10
            cell.text_.text = parentVc?.ingredients
            return cell
        case STEP_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! OneLabel
            cell.label.text = "Steps"
            return cell
        case STEPS:
            let cell = tableView.dequeueReusableCell(withIdentifier: "textItem") as! OneText
            cell.text_.tag = STEPS_TAG
            cell.text_.delegate = self
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.layer.cornerRadius = 10
            cell.text_.text = parentVc?.steps
            return cell
        default:
            return tableView.dequeueReusableCell(withIdentifier: "sectionHeader")!
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        tableView.rowHeight = UITableView.automaticDimension
        tableView.estimatedRowHeight = 600
    }
}
