//
//  AddManualController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/31/22.
//

import UIKit

enum ManualChange {
    case addRecipe
    case link(String?, String?, String, String?)
    case photo(String, String?)
}

class AddManualController: AddDetailController {
    @IBOutlet weak var header: UILabel!

    var change: ManualChange = .addRecipe
    var manualVc: AddManualTableController? = nil
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        let recipeCompletion = { (recipe: ReadableRecipeWithId) in
            DispatchQueue.main.async {
                self.dismiss(animated: true, completion: nil)
            }
            self.navigationVc?.onChange?()
            self.loadRecipe(recipe)
        }
        if let i = manualVc?.ingredients?.nonEmpty() {
            if let n = manualVc?.name?.nonEmpty() {
                addBlob(content: i, name: n, link: manualVc?.link?.nonEmpty(), rawSteps: manualVc?.steps?.nonEmpty()?.components(separatedBy: "\n").compactMap({ $0.nonEmpty() }) ?? [], active: manualVc?.isActive ?? true, completion: recipeCompletion)
            } else {
                alertUnsuccessful("Please provide a name.")
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
                vc.startEditing = false
                vc.link = link
                vc.name = name
                vc.ingredients = ingredients
                vc.steps = steps
            case .photo(let ingredients, let steps):
                vc.startEditing = false
                vc.ingredients = ingredients
                vc.steps = steps
                break
            case .addRecipe:
                vc.startEditing = true
                break
            }
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        switch (change) {
        case .link(_, _, _, _):
            header.text = "Review selections"
            break
        case .photo(_, _):
            header.text = "Review selections"
            break
        case .addRecipe:
            header.text = "Add recipe"
            break
        }
    }
}

class AddManualTableController: UITableViewController, UITextFieldDelegate, UITextViewDelegate {
    var startEditing: Bool = false
    var isActive: Bool = true
    var name: String? = nil
    var link: String? = nil
    var ingredients: String? = nil
    var steps: String? = nil

    let NAME = 0
    let LINK = 1

    let INFO = 0
    let INGREDIENTS = 1
    let STEPS = 2
    
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
        return 3
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case INFO: return 1
        case INGREDIENTS: return 1
        case STEPS: return 1
        default: return 0
        }
    }
    
    override func tableView(_ tableView: UITableView, titleForHeaderInSection section: Int) -> String? {
        switch section {
        case INFO: return "Info"
        case INGREDIENTS: return "Ingredients"
        case STEPS: return "Steps"
        default: return nil
        }
    }
    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case INFO:
            let cell = tableView.dequeueReusableCell(withIdentifier: "infoItem") as! TwoTextOneButton
            cell.button.setImage(UIImage(systemName: isActive ? "checkmark.square" : "square"), for: .normal)
            cell.button.addTarget(self, action: #selector(didTapIsActive), for: .touchUpInside)
            cell.oneText.placeholder = "Recipe name"
            cell.oneText.text = name
            cell.oneText.addDoneButtonOnKeyboard()
            cell.oneText.tag = NAME
            cell.oneText.delegate = self
            cell.twoText.placeholder = "Recipe link (optional)"
            cell.twoText.text = link
            cell.twoText.addDoneButtonOnKeyboard()
            cell.twoText.tag = LINK
            cell.twoText.delegate = self
            if (startEditing) {
                cell.oneText.becomeFirstResponder()
            }
            return cell
        case INGREDIENTS:
            let cell = tableView.dequeueReusableCell(withIdentifier: "textItem") as! OneText
            cell.text_.text = ingredients
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.tag = INGREDIENTS
            cell.text_.delegate = self
            return cell
        case STEPS:
            let cell = tableView.dequeueReusableCell(withIdentifier: "textItem") as! OneText
            cell.text_.text = steps
            cell.text_.addDoneButtonOnKeyboard()
            cell.text_.tag = STEPS
            cell.text_.delegate = self
            return cell
        default:
            return UITableViewCell()
        }
    }
}
