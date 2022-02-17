//
//  RecipeDetailController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 7/5/21.
//

import UIKit

class RecipeDetailController: UIViewController, UITextViewDelegate, UITextFieldDelegate {
    @IBOutlet weak var nameRead: UILabel!
    @IBOutlet weak var nameWrite: UITextField!
    @IBOutlet weak var star1: UIButton!
    @IBOutlet weak var star2: UIButton!
    @IBOutlet weak var star3: UIButton!
    @IBOutlet weak var star4: UIButton!
    @IBOutlet weak var star5: UIButton!
    @IBOutlet weak var link: UIButton!
    
    var recipe: ReadableRecipeWithId? = nil
    var onChange: (() -> Void)? = nil
    var detailVc: RecipeDetailListController? = nil
    
    @IBAction func didTapLink(_ sender: Any?) {
        guard let link = recipe?.recipe.link else { return }
        let url = URL(string: link)!
        if #available(iOS 10.0, *) {
            UIApplication.shared.open(url, options: [:], completionHandler: nil)
        } else {
            UIApplication.shared.openURL(url)
        }
    }
    
    @IBAction func didTapLabel(_ sender: UITapGestureRecognizer) {
        nameWrite.becomeFirstResponder()
        nameWrite.alpha = 1
        nameRead.alpha = 0
    }
    
    func textFieldDidEndEditing(_ textField: UITextField) {
        guard let r = recipe, let name = nameWrite.text else { return }
        nameWrite.alpha = 0
        nameWrite.resignFirstResponder()
        nameRead.text = name
        nameRead.alpha = 1
        updateRecipe(id: r.id, recipe: ReadableRecipe(name: name, link: r.recipe.link, active: r.recipe.active, rating: r.recipe.rating, notes: r.recipe.notes, ingredients: r.recipe.ingredients, steps: r.recipe.steps))
        onChange?()
    }
    
    private func didTapStar(which: Int) {
        let filledStar = UIImage(systemName: "star.fill")
        let unfilledStar = UIImage(systemName: "star")
        guard let r = recipe else { return }
        switch which {
        case 0:
            star1.setImage(unfilledStar, for: .normal)
            star2.setImage(unfilledStar, for: .normal)
            star3.setImage(unfilledStar, for: .normal)
            star4.setImage(unfilledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        case 1:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(unfilledStar, for: .normal)
            star3.setImage(unfilledStar, for: .normal)
            star4.setImage(unfilledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        case 2:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(filledStar, for: .normal)
            star3.setImage(unfilledStar, for: .normal)
            star4.setImage(unfilledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        case 3:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(filledStar, for: .normal)
            star3.setImage(filledStar, for: .normal)
            star4.setImage(unfilledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        case 4:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(filledStar, for: .normal)
            star3.setImage(filledStar, for: .normal)
            star4.setImage(filledStar, for: .normal)
            star5.setImage(unfilledStar, for: .normal)
        default:
            star1.setImage(filledStar, for: .normal)
            star2.setImage(filledStar, for: .normal)
            star3.setImage(filledStar, for: .normal)
            star4.setImage(filledStar, for: .normal)
            star5.setImage(filledStar, for: .normal)
        }
        if which != r.recipe.rating && which > 0 {
            let newRecipe = ReadableRecipe(name: r.recipe.name, link: r.recipe.link, active: r.recipe.active, rating: which, notes: r.recipe.notes, ingredients: r.recipe.ingredients, steps: r.recipe.steps)
            updateRecipe(id: r.id, recipe: newRecipe)
            loadData()
            onChange?()
            DispatchQueue.main.async {
                self.view.reloadInputViews()
            }
        }
    }
    
    @IBAction func didTapStar1(_ sender: Any?) {
        didTapStar(which: 1)
    }
    
    @IBAction func didTapStar2(_ sender: Any?) {
        didTapStar(which: 2)
    }
    
    @IBAction func didTapStar3(_ sender: Any?) {
        didTapStar(which: 3)
    }
    
    @IBAction func didTapStar4(_ sender: Any?) {
        didTapStar(which: 4)
    }
    
    @IBAction func didTapStar5(_ sender: Any?) {
        didTapStar(which: 5)
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        guard let r = recipe else { return }
        if let vc = segue.destination as? RecipeDetailListController, segue.identifier == "embedDetail" {
            detailVc = vc
            loadData()
            vc.recipe = r
            vc.steps = r.recipe.steps.map({ StepWithId(id: $0, step: $1) }).sorted(by: { $0.step.order < $1.step.order })
            vc.ingredients = r.recipe.ingredients.map({ ReadableIngredientWithId(id: $0, ingredient: $1) }).sorted(by: { $0.ingredient.order < $1.ingredient.order })
            vc.onChange = { () -> Void in
                self.loadData()
                self.onChange?()
            }
        }
    }
    
    private func loadData() {
        guard let r = recipe else { return }
        guard let newRecipe = getRecipe(id: r.id) else { return }
        detailVc?.recipe = newRecipe
        detailVc?.steps = newRecipe.recipe.steps.map({ StepWithId(id: $0, step: $1) }).sorted(by: { ($0.step.order, $0.step.step) < ($1.step.order, $1.step.step) })
        detailVc?.ingredients = newRecipe.recipe.ingredients.map({ ReadableIngredientWithId(id: $0, ingredient: $1) }).sorted(by: { ($0.ingredient.order, $0.ingredient.name) < ($1.ingredient.order, $1.ingredient.name) })
        DispatchQueue.main.async {
            self.detailVc?.tableView.reloadData()
        }
    }
    
    override func viewDidLoad() {
        nameRead.text = recipe?.recipe.name
        nameWrite.text = recipe?.recipe.name
        if recipe?.recipe.link == nil {
            link.removeFromSuperview()
        }
        didTapStar(which: recipe?.recipe.rating ?? 0)
        nameWrite.addDoneButtonOnKeyboard()
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        loadData()
    }
}
